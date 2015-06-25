module Logary.Heka.Client

open System
open System.IO
open ProtoBuf
open Logary.Heka.Messages

module Encoder =
  let encode (s : Stream) (m : Message) =
    Serializer.Serialize(s, m)