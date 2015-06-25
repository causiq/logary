namespace Logary.Heka

open System
open System.Text
open System.Security.Cryptography
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
      // default to SHA1
      hash    = HmacHashFunction.SHA1
      key     = ""
      version = 0u }

  static member Create(name, key, ?hashAlgo, ?version) =
    { name    = name
      hash    = defaultArg hashAlgo HmacHashFunction.SHA1
      key     = key
      version = defaultArg version 0u }

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
