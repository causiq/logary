module Logary.Heka.Client

open System
open System.IO
open System.Security.Cryptography
open ProtoBuf
open Logary.ProtoBufUtils
open Logary.Heka
open Logary.Heka.Messages

// Async.Bind : Task -> ...
open Hopac

type EncodingError =
  | HeaderTooLarge of string
  | MessageTooLarge of string

module Encoder =
  let fromZero (s : Stream) f =
    s.Seek(0L, SeekOrigin.Begin) |> ignore
    f s

  let largeHeader headerSize =
    sprintf "Message header too big, requires %d (MAX_HEADER_SIZE = %d)"
            headerSize Constants.MaxHeaderSize

  let largeMessage messageSize maxMessageSize =
    sprintf "Message too big, requires %d (MAX_MESSAGE_SIZE = %d)"
            messageSize
            maxMessageSize

  let encodeMessage (s : Stream) (m : Message) =
    Serializer.Serialize(s, m)

  let encodeHeader (s : Stream) (h : Header) =
    Serializer.Serialize(s, h)

  let encode (conf : HekaConfig) (s : Stream) (m : Message) =
    let msgMs = new MemoryStream()
    encodeMessage msgMs m
    if msgMs.Length > int64 conf.maxMessageSize then
      let msg = largeMessage (msgMs.Length) conf.maxMessageSize
      use x = msgMs
      Choice2Of2 (MessageTooLarge msg)
    else
      let header =
        Option.fold (fun (s : Header) (t : MessageSigningConfig) ->
          let algo = t.hashAlgo ()
          s.hmac_hash_function <- (if t.hash = HmacHashFunction.MD5 then None else Some t.hash)
                                  |> Nullable.ofOption
          s.hmac_signer        <- t.name
          s.hmac_key_version   <- Nullable t.version
          s.hmac               <- fromZero msgMs algo.ComputeHash
          s)
          (Header(message_length = uint32 (msgMs.Length)))
          conf.signingConfig

      let hdrMs = new MemoryStream()
      encodeHeader hdrMs header
      if hdrMs.Length > int64 Constants.MaxHeaderSize then
        let msg = largeHeader (hdrMs.Length)
        use x = msgMs
        use x = hdrMs
        Choice2Of2 (HeaderTooLarge msg)
      else
        Choice1Of2 (job {
          try
            use msgMs = msgMs
            use hdrMs = hdrMs
            printfn "write record separator"
            do s.WriteByte Constants.RecordSeparator
            do! Async.Sleep 200
            printfn "write header length"
            do s.WriteByte (byte (hdrMs.Length))
            do! Async.Sleep 200
            printfn "write header"
            do! fromZero hdrMs (fun ms -> ms.CopyToAsync s)
            do! Async.Sleep 200
            printfn "write unit separator"
            do s.WriteByte Constants.UnitSeparator
            do! Async.Sleep 200
            printfn "write unit message"
            do! fromZero msgMs (fun ms -> ms.CopyToAsync s)
          with e -> printfn "BAM!! %A" e
        })
