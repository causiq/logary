module Logary.Logenties.LeClient

open System
open System.IO
open System.Text
open System.Net.Sockets
open System.Net.Security
open System.Security.Cryptography.X509Certificates

open Logary.Internals.Tcp

[<Literal>]
let ApiDomain = "api.logentries.com"

/// Port number for token logging on Logentries API server.
[<Literal>]
let ApiInsecurePort = 10000

/// Port number for TLS encrypted token logging on Logentries API server
[<Literal>]
let ApiTLSPort = 20000

/// Logentries API server certificate.
let ApiCert =
  "-----BEGIN CERTIFICATE-----
MIIE3jCCA8agAwIBAgICGbowDQYJKoZIhvcNAQELBQAwZjELMAkGA1UEBhMCVVMx
FjAUBgNVBAoTDUdlb1RydXN0IEluYy4xHTAbBgNVBAsTFERvbWFpbiBWYWxpZGF0
ZWQgU1NMMSAwHgYDVQQDExdHZW9UcnVzdCBEViBTU0wgQ0EgLSBHNDAeFw0xNDEw
MjkxMjI5MzJaFw0xNjA5MTQwODE3MzlaMIGWMRMwEQYDVQQLEwpHVDAzOTM4Njcw
MTEwLwYDVQQLEyhTZWUgd3d3Lmdlb3RydXN0LmNvbS9yZXNvdXJjZXMvY3BzIChj
KTEyMS8wLQYDVQQLEyZEb21haW4gQ29udHJvbCBWYWxpZGF0ZWQgLSBRdWlja1NT
TChSKTEbMBkGA1UEAxMSYXBpLmxvZ2VudHJpZXMuY29tMIIBIjANBgkqhkiG9w0B
AQEFAAOCAQ8AMIIBCgKCAQEAyvDKhaiboZS5GHaZ7HBsidUBJoBu1YqMgUxvFohv
xppf5QqjjDP4knjKyC3K8t7cMTFem1CXHA03AW0nImy2cbDcWhr7MpTr5J90e3Ld
neWfBiFNStzjaE9jhdWDvu0ctVact1TIQgYfSAlRMEKW+OuaUwq3dEJNRJNzdrzE
aefQN7c4e2IgTuFvU9p7Qzifiq9Qu1VoSSDK3lxZiQuChWtd4sGYhqqjbkkMRvQ/
pRdiJ0gcFtGaqZLaj3Op+poz40iOiubWB4U8iOHiSjoGdRVi0LJKUeiSRw9lRO+1
qbj4g9ASZU+g7XugZn5GQvrR8E6ha5nZHEdDTI8JiEHXLwIDAQABo4IBYzCCAV8w
HwYDVR0jBBgwFoAUC1Dsd+8qm//sA6EK/63G5CoYxz4wVwYIKwYBBQUHAQEESzBJ
MB8GCCsGAQUFBzABhhNodHRwOi8vZ3Uuc3ltY2QuY29tMCYGCCsGAQUFBzAChhpo
dHRwOi8vZ3Uuc3ltY2IuY29tL2d1LmNydDAOBgNVHQ8BAf8EBAMCBaAwHQYDVR0l
BBYwFAYIKwYBBQUHAwEGCCsGAQUFBwMCMB0GA1UdEQQWMBSCEmFwaS5sb2dlbnRy
aWVzLmNvbTArBgNVHR8EJDAiMCCgHqAchhpodHRwOi8vZ3Uuc3ltY2IuY29tL2d1
LmNybDAMBgNVHRMBAf8EAjAAMFoGA1UdIARTMFEwTwYKYIZIAYb4RQEHNjBBMD8G
CCsGAQUFBwIBFjNodHRwczovL3d3dy5nZW90cnVzdC5jb20vcmVzb3VyY2VzL3Jl
cG9zaXRvcnkvbGVnYWwwDQYJKoZIhvcNAQELBQADggEBAGL2wkx4Gk99EAcW0ClG
sCVFUbZ/DW2So0c5MjKkfFIGdH4a++x9eTNi28GoeF6YF2S8tOKS4fHHHxby4Fvn
ToUp4yR3Z3zAwNFULC1Gc+1kaV0/6k99LuiKNlIU7CHocSjQs7zvmc85l152lrAL
pzodvnfOn8rjUZvGOi2hb8VC7ZUSQCD9NJNNexF6G4dYc2TBjCD5xrhYXNcYCDXu
TGtvFnmBzFIO06IjqPWUFnerZxkktHf63PCB+xTxRWtDc84K91jmc+u7k/yY5wdf
aigW0/FPgSXR+as3fD1SSLuIgHynDdsUYLtCdbqiIRpZc/cmXzJI0bzhzpgGDPcn
81I=
-----END CERTIFICATE-----"
  |> Encoding.UTF8.GetBytes
  |> fun bs -> new X509Certificate2(bs)

/// The default verification callback, see https://logentries.com/doc/certificates/
let certVerifyCb
  (sender : obj)
  (cert : X509Certificate)
  (chain : X509Chain)
  (errors : SslPolicyErrors) =
  cert.GetCertHashString() = ApiCert.GetCertHashString()

/// Create a new TcpClient and SslStream that can talk with the API
let create () =
  let useTLS = true
  let port = if useTLS then ApiTLSPort else ApiInsecurePort
  let client = new TcpClient(ApiDomain, port)
  client.NoDelay <- true
  let stream = client.GetStream()
  let cb = new RemoteCertificateValidationCallback(certVerifyCb)
  let sslStream = new SslStream(stream, false, cb)
  sslStream.AuthenticateAsClient ApiDomain
  client, sslStream

let send (msg : byte []) (stream : Stream) flush = async {
  use ms = new MemoryStream(msg)
  do! transfer msg.Length ms stream
  if flush then
    stream.Flush()
  }

// Reference from Logentries (not same code though):
// https://github.com/logentries/le_dotnet/blob/master/src/LogentriesCore/AsyncLogger.cs
