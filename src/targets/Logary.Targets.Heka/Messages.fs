module Logary.Heka.Messages

open System
open System.Collections.Generic
open System.ComponentModel
open NodaTime
open ProtoBuf
open Logary.ProtoBufUtils

type HmacHashFunction =
  | MD5 = 0
  | SHA1 = 1

[<ProtoContract>]
type Header =
  [<ProtoMember(1, IsRequired=true)>] 
  val mutable message_length : uint32

  [<ProtoMember(3, IsRequired=false)>]
  val mutable hmac_hash_function : HmacHashFunction

  [<ProtoMember(4, IsRequired=false)>]
  val mutable hmac_signer : string

  [<ProtoMember(5, IsRequired=false)>]
  val mutable hmac_key_version : uint32

  [<ProtoMember(6, IsRequired=false)>]
  val mutable hmac : byte []

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
  val mutable value_type : ValueType

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

[<ProtoContract>]
type Message =
  [<ProtoMember(1, IsRequired=true)>] 
  val mutable uuid : byte []

  /// Nanoseconds since unix epoch
  [<ProtoMember(2, IsRequired=true)>] 
  val mutable timestamp : int64

  [<ProtoMember(3, IsRequired=false)>] 
  val mutable ``type`` : string

  [<ProtoMember(4, IsRequired=false)>] 
  val mutable logger : string

  [<ProtoMember(5, IsRequired=false); DefaultValue 7>] 
  val mutable severity : int32

  [<ProtoMember(6, IsRequired=false)>] 
  val mutable payload : string

  [<ProtoMember(7, IsRequired=false)>] 
  val mutable env_version : string

  [<ProtoMember(8, IsRequired=false)>] 
  val mutable pid : int32

  [<ProtoMember(9, IsRequired=false)>] 
  val mutable hostname : string

  [<ProtoMember(10, IsRequired=true)>] 
  val mutable fields : Field List
