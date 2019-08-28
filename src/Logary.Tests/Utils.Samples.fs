namespace Logary.Tests

open System
open System.Runtime.CompilerServices

type SampleObject() =
  member x.PropA =
    45
  member x.PropB =
    raise (Exception ("Oh noes, no referential transparency here"))

type Domain =
  Domain of hiera:string[]
with
  member x.value =
    let (Domain values) = x
    String.concat "." values
/// Sample
type RootEx(m) =
  inherit Exception(m)
/// Sample
type ChildEx(m) =
  inherit RootEx(m)
  member x.MyValue = 42
exception MyFSharpExn of epoch:int64

type Tenant =
  { tenantId: string
    permissions: string }

[<AutoOpen>]
module Utils =
  open Logary
  open NodaTime

  let timeMessage (nanos: int64) level =
    let value, units = float nanos, Scaled (Seconds, float Constants.NanosPerSecond)
    let name = PointName.parse "A.B.C"
    Message.gaugeWithUnit name "Check" (Gauge (Float value, units))
    |> Message.setLevel level

  let gaugeMessage (value: float) level =
    let name = PointName.parse "Revolver"
    Message.gaugeWithUnit name "spin" (Gauge (Float value, (Div (Seconds, Units.Other "revolution"))))
    |> Message.setLevel level

  let multiGaugeMessage level =
    Message.event level "Processor.% Idle"
    |> Message.addGauges [
      "Core 1", (Gauge (Fraction (1L, 1000L), Percent))
      "Core 2", (Gauge (Float 0.99, Percent))
      "Core 3", (Gauge (Float 0.473223755, Percent))
    ]
    |> Message.setContext "host" "db-001"
    |> Message.setContext "service" "api-web"

  [<MethodImpl(MethodImplOptions.NoInlining)>]
  let innermost (throwCLRExn: bool) =
    if throwCLRExn then
      raise (Exception "Bad things going on")
    else
      raise (MyFSharpExn 42L)

  [<MethodImpl(MethodImplOptions.NoInlining)>]
  let middleWay throwCLRExn =
    1 + 3 |> ignore
    innermost throwCLRExn

  [<MethodImpl(MethodImplOptions.NoInlining)>]
  let withException f =
    try
      middleWay true
    with e ->
      f e

  [<MethodImpl(MethodImplOptions.NoInlining)>]
  let withFSharpExn f =
    try
      middleWay false
    with e ->
      f e

  let raisedExn msg =
    let e = ref None: exn option ref
    try raise <| ApplicationException(msg)
    with ex -> e := Some ex
    (!e).Value

  let raisedExnWithInner msg inner =
    let e = ref None: exn option ref
    try raise <| ApplicationException(msg,inner)
    with ex -> e := Some ex
    (!e).Value

  let exnMsg =
    Message.event Error "Unhandled exception"
    |> Message.setNameStr "Logary.Tests"
    |> Message.setField "tenant" { tenantId = "12345"; permissions = "RWX" }
    |> Message.setContextFromMap (Map
      [ "user", box (Map
          [ "name", box "haf"
            "id", box "deadbeef234567"
          ])
      ])
    |> withException Message.addExn

  let helloWorld =
    Message.eventX "Hello World!"
    >> Message.setTicksEpoch (0L: EpochNanoSeconds)

  let helloWorldTS =
    helloWorld
    >> fun m ->
        let now = SystemClock.Instance.GetCurrentInstant()
        { m with value = sprintf "%s @ %O" m.value now }
