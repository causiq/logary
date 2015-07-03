namespace Logary.Heka

open System
open System.Net
open System.Net.Security
open System.Text
open System.Security.Cryptography
open System.Security.Cryptography.X509Certificates
open Logary.Heka.Messages
open Logary.Heka.Constants

type MessageSigningConfig =
  { name    : string
    hash    : HmacHashFunction
    key     : string
    version : uint32 }

  member x.hashAlgo () =
    let algo =
      match x.hash with
      | HmacHashFunction.MD5 -> new HMACMD5() :> HMAC
      | HmacHashFunction.SHA1 -> new HMACSHA1() :> HMAC
      | _
      | HmacHashFunction.SHA256 -> new HMACSHA256() :> HMAC
    algo.Key <- Encoding.UTF8.GetBytes x.key
    algo

  static member Empty =
    { name    = ""
      // default to HMACSHA1, the strongest HMAC algo supported by Heka
      hash    = HmacHashFunction.SHA1
      key     = ""
      version = 0u }

  static member Create(name, key, ?hashAlgo, ?version) =
    { name    = name
      hash    = defaultArg hashAlgo HmacHashFunction.SHA1
      key     = key
      version = defaultArg version 0u }

type UseTLS = bool

type HekaConfig =
  { maxMessageSize : uint32
    endpoint       : IPEndPoint * UseTLS
    caValidation   : X509Certificate -> X509Chain -> SslPolicyErrors -> bool
    signingConfig  : MessageSigningConfig option }

  member x.maxRecordSize =
    HeaderFramingSize
    + MaxHeaderSize
    + x.maxMessageSize

  static member Empty =
    let defaultCaValidation : X509Certificate -> X509Chain -> SslPolicyErrors -> bool =
      fun cer chn err ->
        ServicePointManager.ServerCertificateValidationCallback.Invoke(null, cer, chn, err)

    { maxMessageSize = uint32 (UInt16.MaxValue) + 1u // 64 * 1024
      // https://hekad.readthedocs.org/en/latest/config/inputs/tcp.html
      endpoint       = IPEndPoint(IPAddress.Loopback, 5565), false
      caValidation   = defaultCaValidation
      signingConfig  = None }