module Logary.Heka.Messages

// About schema:
// - https://github.com/mozilla-services/heka/issues/1588
// - https://hekad.readthedocs.org/en/latest/message/index.html

open System
open System.Collections.Generic
open System.ComponentModel
open NodaTime
open ProtoBuf
open Logary.ProtoBufUtils

type HmacHashFunction =
  | MD5 = 0
  | SHA1 = 1
  | SHA256 = 2

[<ProtoContract>]
type Header =
  [<ProtoMember(1, IsRequired=true)>] 
  val mutable message_length : uint32

  [<ProtoMember(3, IsRequired=false)>]
  val mutable hmac_hash_function : HmacHashFunction Nullable

  [<ProtoMember(4, IsRequired=false)>]
  val mutable hmac_signer : string

  [<ProtoMember(5, IsRequired=false)>]
  val mutable hmac_key_version : uint32 Nullable

  [<ProtoMember(6, IsRequired=false)>]
  val mutable hmac : byte []

  new () =
    { message_length = 0u
      hmac_hash_function = Nullable ()
      hmac_signer = null
      hmac_key_version = Nullable ()
      hmac = null }

type ValueType =
  | STRING  = 0
  | BYTES   = 1
  | INTEGER = 2
  | DOUBLE  = 3
  | BOOL    = 5

[<ProtoContract>]
type Field =
  [<ProtoMember(1, IsRequired=true)>] 
  val mutable name : string

  [<ProtoMember(2, IsRequired=false)>] 
  val mutable value_type : ValueType Nullable

  [<ProtoMember(3, IsRequired=false)>] 
  val mutable representation : string

  [<ProtoMember(4, IsRequired=false)>] 
  val mutable value_string : string List

  [<ProtoMember(5, IsRequired=false)>] 
  val mutable value_bytes : byte [] List

  [<ProtoMember(6, IsRequired=false, IsPacked = true)>] 
  val mutable value_integer : int64 List

  [<ProtoMember(7, IsRequired=false, IsPacked = true)>] 
  val mutable value_double : float List

  [<ProtoMember(8, IsRequired=false, IsPacked = true)>] 
  val mutable value_bool : bool List

  new () =
    { name = ""
      value_type = Nullable ()
      representation = null
      value_string = null
      value_bytes  = null
      value_integer = null
      value_double = null
      value_bool = null }

  new (name, typ, rep, strs) =
    { name = name
      value_type = typ
      representation = rep
      value_string = List (strs : _ seq)
      value_bytes  = null
      value_integer = null
      value_double = null
      value_bool = null }

  new (name, typ, rep, bs) =
    { name = name
      value_type = typ
      representation = rep
      value_string = null
      value_bytes  = List (bs : _ seq)
      value_integer = null
      value_double = null
      value_bool = null }

  new (name, typ, rep, ints) =
    { name = name
      value_type = typ
      representation = rep
      value_string = null
      value_bytes  = null
      value_integer = List (ints : _ seq)
      value_double = null
      value_bool = null }

  new (name, typ, rep, fs) =
    { name = name
      value_type = typ
      representation = rep
      value_string = null
      value_bytes  = null
      value_integer = null
      value_double = List (fs : _ seq)
      value_bool = null }

  new (name, typ, rep, bs) =
    { name = name
      value_type = typ
      representation = rep
      value_string = null
      value_bytes  = null
      value_integer = null
      value_double = null
      value_bool = List (bs : _ seq) }

[<ProtoContract>]
type Message =
  [<ProtoMember(1, IsRequired=true)>] 
  val mutable uuid : byte []

  /// Nanoseconds since unix epoch
  [<ProtoMember(2, IsRequired=true)>] 
  val mutable timestamp : int64

  /// heka.logary
  [<ProtoMember(3, IsRequired=false)>] 
  val mutable ``type`` : string

  [<ProtoMember(4, IsRequired=false)>] 
  val mutable logger : string

  [<ProtoMember(5, IsRequired=false); DefaultValue 7>] 
  val mutable severity : int32 Nullable

  [<ProtoMember(6, IsRequired=false)>] 
  val mutable payload : string

  [<ProtoMember(7, IsRequired=false)>] 
  val mutable env_version : string

  [<ProtoMember(8, IsRequired=false)>] 
  val mutable pid : int32 Nullable

  [<ProtoMember(9, IsRequired=false)>] 
  val mutable hostname : string

  [<ProtoMember(10, IsRequired=false)>]
  val mutable fields : Field List

  new () =
    { uuid        = null
      timestamp   = 0L
      ``type``    = null // "heka.logary" // e.g. "stat.cpu_time"
      logger      = null // LogLine.path
      severity    = Nullable () //(7).n
      payload     = null
      env_version = null // Logary.Internals.Lib.LogaryVersion
      pid         = Nullable ()
      hostname    = null
      fields      = null }

  new (uuid : Guid, ts, tp, logger, severity, payload, envVer, pid, hostname, fields) =
    { uuid        = uuid.ToByteArray()
      timestamp   = ts
      ``type``    = tp
      logger      = logger
      severity    = severity
      payload     = payload
      env_version = envVer
      pid         = pid |> Option.toNullable
      hostname    = hostname
      fields      = fields }