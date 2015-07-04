module Logary.Riemann.Messages

open System
open System.ComponentModel
open System.Collections.Generic

open NodaTime

open ProtoBuf
open Logary.ProtoBufUtils

// # https://github.com/aphyr/riemann-java-client/commit/81dd51349cfcd4daf6b9ff80aa59f4aecbcc078c

// Because protocol buffers is strongly typed, the metric of an event is
// represented as one of metric_d (floating point 64-bit), metric_f (floating
// point 32-bit), or metric_sint64 (64-bit signed integer). Your client should
// emit and consume all of these types.

// TODO: You can also query events from the index using a basic query language.
// Just submit a Message with your query in message.query.string. Search queries
// will return a message with repeated Events matching that expression. A null
// expression will return no states. For some example queries, see The query
// test suite.


[<ProtoContract>] // ; CustomEquality; CustomComparison
type State =
  /// Event.time is the time in unix epoch seconds and is optional
  /// The server will generate a time for each event when received if you do
  /// not provide one.
  [<ProtoMember(1, IsRequired=false); DefaultValue 0L>] 
  val mutable time : int64 Nullable

  [<ProtoMember(2, IsRequired=false); DefaultValue "">]
  val mutable state : string

  /// Events are uniquely identified by host and service. Both allow null.
  [<ProtoMember(3, IsRequired=false); DefaultValue "">]
  val mutable service : string

  /// Events are uniquely identified by host and service. Both allow null.
  [<ProtoMember(4, IsRequired=false); DefaultValue "">]
  val mutable host : string

  [<ProtoMember(5, IsRequired=false); DefaultValue "">]
  val mutable description : string

  [<ProtoMember(6, IsRequired=false); DefaultValue false>]
  val mutable once : bool Nullable

  [<ProtoMember 7>]
  val mutable tags : string List

  [<ProtoMember(8, IsRequired=false); DefaultValue 0.>]
  val mutable ttl : float Nullable

  new () =
    { time        = 0L.n
      state       = ""
      service     = ""
      host        = ""
      description = ""
      once        = false.n
      tags        = List ()
      ttl         = (0.).n }

  new (time, state, service, host, description, once, tags, ttl) =
    { time        = Nullable time
      state       = state
      service     = service
      host        = host
      description = description
      once        = Nullable once
      tags        = List (tags : _ seq)
      ttl         = Nullable ttl }

  override x.ToString() =
    sprintf "State(time=%A, state=%s, service=%s, host=%s, description=%s, once=%A, tags=%A, ttl=%A)"
      x.time x.state x.service x.host x.description x.once x.tags x.ttl

  override x.Equals other =
    match other with
    | :? State as tother -> (x :> IEquatable<State>).Equals tother
    | _ -> false

  override x.GetHashCode () =
    hash x.time
    ^^^ 293 * hash x.state
    ^^^ 293 * hash x.service
    ^^^ 293 * hash x.host
    ^^^ 293 * hash x.description
    ^^^ 293 * hash x.once
    ^^^ 293 * hash x.tags
    ^^^ 293 * hash x.ttl

  interface IComparable with
    member x.CompareTo other =
      match other with
      | null        -> 1
      | :? State as tother ->
        (x :> IComparable<State>).CompareTo tother
      | _ -> failwith <| sprintf "invalid comparison 'State' to %A" other

  interface IComparable<State> with
    member x.CompareTo other =
      compare x.time other.time
      |> compareElse x.state other.state

  interface IEquatable<State> with
    member x.Equals other =
      x.time               =? other.time
      && x.state           = other.state
      && x.service         = other.service
      && x.host            = other.host
      && x.description     = other.description
      && x.once            =? other.once
      && List.ofSeq x.tags = List.ofSeq other.tags
      && x.ttl             =? other.ttl

[<ProtoContract>]
type Attribute =
  [<ProtoMember(1, IsRequired=true); DefaultValue "">]
  val mutable key : string

  [<ProtoMember(2, IsRequired=false); DefaultValue "">]
  val mutable value : string

  new () =
    { key   = ""
      value = "" }

  new (key, value) =
    { key   = key
      value = value }

  override x.ToString() =
    sprintf "Attribute(%s: %s)" x.key x.value

  override x.Equals other =
    match other with
    | :? Attribute as tother -> (x :> IEquatable<Attribute>).Equals tother
    | _ -> false

  override x.GetHashCode () =
    hash x.key ^^^ 293 * hash x.value

  interface IComparable with
    member x.CompareTo other =
      match other with
      | null        -> 1
      | :? Attribute as tother ->
        (x :> IComparable<Attribute>).CompareTo tother
      | _ -> failwith <| sprintf "invalid comparison 'Attribute' to %A" other

  interface IComparable<Attribute> with
    member x.CompareTo other =
      compareCore x.key other.key
      |> compareElse x.value other.value

  interface IEquatable<Attribute> with
    member x.Equals other =
      x.key      = other.value
      && x.value = other.value

[<ProtoContract>]
type Event =
  [<ProtoMember(1, IsRequired=false); DefaultValue 0L>]
  val mutable time : int64 Nullable

  [<ProtoMember(2, IsRequired=false); DefaultValue "">]
  val mutable state : string

  [<ProtoMember(3, IsRequired=false); DefaultValue "">]
  val mutable service : string

  [<ProtoMember(4, IsRequired=false); DefaultValue "">]
  val mutable host : string

  [<ProtoMember(5, IsRequired=false); DefaultValue "">]
  val mutable description : string

  [<ProtoMember(7)>]
  val mutable tags : string List

  [<ProtoMember(8, IsRequired=false); DefaultValue null>]
  val mutable ttl : single Nullable // F# single = proto float

  [<ProtoMember 9>]
  val mutable attributes : Attribute List

  [<ProtoMember(13, IsRequired=false); DefaultValue(0L)>]
  val mutable metric_sint64 : int64 Nullable // F# int64 = proto signed int64

  [<ProtoMember(14, IsRequired=false); DefaultValue(Double.NaN)>]
  val mutable metric_d : float Nullable // F# float = proto double
  
  [<ProtoMember(15, IsRequired=false); DefaultValue(Single.NaN)>]
  val mutable metric_f : float32 Nullable

  new () =
    { time          = Nullable (SystemClock.Instance.Now.Ticks / NodaConstants.TicksPerSecond)
      state         = ""
      service       = ""
      host          = ""
      description   = ""
      tags          = List()
      ttl           = Nullable()
      attributes    = List()
      metric_sint64 = Nullable()
      metric_d      = Nullable()
      metric_f      = Nullable() }

  new (time, state, service, host, description, tags, ttl, attributes) =
    { time          = Nullable time
      state         = state
      service       = service
      host          = host
      description   = description
      tags          = List (tags : _ seq)
      ttl           = Nullable ttl
      attributes    = List (attributes : _ seq)
      metric_sint64 = Nullable ()
      metric_d      = Nullable ()
      metric_f      = Nullable () }

  static member CreateInt64(value, time, state, service, host, description, tags, ttl, attributes) =
    let x = Event(time, state, service, host, description, tags, ttl, attributes)
    x.metric_sint64 <- Nullable value
    x

  static member CreateDouble(value, time, state, service, host, description, tags, ttl, attributes) =
    let x = Event(time, state, service, host, description, tags, ttl, attributes)
    x.metric_d <- Nullable value
    x

  static member CreateSingle(value, time, state, service, host, description, tags, ttl, attributes) =
    let x = Event(time, state, service, host, description, tags, ttl, attributes)
    x.metric_f <- Nullable value
    x

  override x.ToString() =
    sprintf "Event(time=%O, state=%s, service=%s, host=%s, description=%s, tags=%A, ttl=%O, attributes=%A, metric_sint64=%O, metric_d=%O, metric_f=%O)"
      x.time x.state x.service x.host x.description x.tags x.ttl x.attributes
      x.metric_sint64 x.metric_d x.metric_f

  override x.Equals other =
    match other with
    | :? Event as tother -> (x :> IEquatable<Event>).Equals tother
    | _ -> false

  override x.GetHashCode () =
    hash x.time
    ^^^ 293 * hash x.state
    ^^^ 293 * hash x.service
    ^^^ 293 * hash x.host
    ^^^ 293 * hash x.description
    ^^^ 293 * hash x.tags
    ^^^ 293 * hash x.ttl
    ^^^ 293 * hash x.attributes
    ^^^ 293 * hash x.metric_sint64
    ^^^ 293 * hash x.metric_d
    ^^^ 293 * hash x.metric_f

  interface IComparable with
    member x.CompareTo other =
      match other with
      | null        -> 1
      | :? Event as tother ->
        (x :> IComparable<Event>).CompareTo tother
      | _ -> failwith <| sprintf "invalid comparison 'Event' to %A" other

  interface IComparable<Event> with
    member x.CompareTo other =
      compare x.time other.time
      |> compareElse x.state other.state

  interface IEquatable<Event> with
    member x.Equals other =
      x.time               =? other.time
      && x.state           = other.state
      && x.service         = other.service
      && x.host            = other.host
      && x.description     = other.description
      && List.ofSeq x.tags = List.ofSeq other.tags
      && x.ttl             =? other.ttl
      && List.ofSeq x.attributes = List.ofSeq other.attributes
      && x.metric_sint64   =? other.metric_sint64
      && x.metric_d        =? other.metric_d
      && x.metric_f        =? other.metric_f

[<ProtoContract>]
type Query =
  [<ProtoMember 1; DefaultValue "">]
  val mutable ``string`` : string

  new () = { ``string`` = "" }
  new str = { ``string`` = str }

  override x.ToString() =
    sprintf "Query(%s)" x.string

  override x.Equals other =
    match other with
    | :? Query as tother -> (x :> IEquatable<Query>).Equals tother
    | _ -> false

  override x.GetHashCode () =
    hash x.string

  interface IEquatable<Query> with
    member x.Equals other =
      x.string = other.string

[<ProtoContract>]
type Msg =
  [<ProtoMember(2, IsRequired = false); DefaultValue false>]
  val mutable ok     : bool

  [<ProtoMember(3, IsRequired = false); DefaultValue "">]
  val mutable error  : string

  [<ProtoMember(4)>]
  val mutable states : State List

  [<ProtoMember(5, IsRequired = false)>]
  val mutable query  : Query

  [<ProtoMember(6)>]
  val mutable events : Event List

  new () =
    { ok     = false
      error  = ""
      states = List<_>()
      query  = Unchecked.defaultof<Query>
      events = List<_>() }

  new (ok, error, states, query, events) =
    { ok     = ok
      error  = error
      states = List<_>(states : _ seq)
      query  = query
      events = List<_>(events : _ seq) }

  override x.ToString() =
    sprintf "Msg(ok=%b, error=%s, states=%A, query=%A, events=%A)"
      x.ok x.error x.states x.query x.events

  override x.Equals other =
    match other with
    | :? Msg as tother -> (x :> IEquatable<Msg>).Equals tother
    | _ -> false

  override x.GetHashCode () =
    hash x.ok
    ^^^ 293 * hash x.error
    ^^^ 293 * hash x.states
    ^^^ 293 * hash x.query
    ^^^ 293 * hash x.events

  interface IEquatable<Msg> with
    member x.Equals other =
      x.ok                   = other.ok
      && x.error             = other.error
      && List.ofSeq x.states = List.ofSeq other.states
      && x.query             = other.query
      && List.ofSeq x.events = List.ofSeq other.events