namespace Logary.Heka

open System
open System.Security.Cryptography
open Logary.Heka.Messages
open Logary.Heka.Constants

type MessageSigningConfig =
  { name    : string
    hash    : HmacHashFunction
    key     : string
    version : uint32 }

  member x.hashAlgo () =
    match x.hash with
    | HmacHashFunction.MD5 -> HMACMD5.Create()
    | _
    | HmacHashFunction.SHA1 -> HMACSHA1.Create()

  static member Empty =
    { name    = ""
      // default to SHA1
      hash    = HmacHashFunction.SHA1
      key     = ""
      version = 0u }

type HekaConfig =
  { maxMessageSize : uint32
    signingConfig  : MessageSigningConfig option }

  member x.maxRecordSize =
    HeaderFramingSize
    + MaxHeaderSize
    + x.maxMessageSize

  static member Empty =
    { maxMessageSize = UInt32.MaxValue // 64 * 2014
      signingConfig  = None }
