namespace Logary.Tests

open System
open System.Runtime.CompilerServices
open Logary

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
  let timeMessage (nanos: int64) level =
    let value, units = float nanos, U.Scaled (U.Seconds, float Constants.NanosPerSecond)
    let g = Gauge (Value.Float value, units)
    Model.GaugeMessage(g, Map [ "gauge_name", "A.B.C" ], level=level)

  let gaugeMessage (value: float) level =
    let g = Gauge (Value.Float value, (U.Div (U.Seconds, U.Other "revolution")))
    Model.GaugeMessage(g, Map [ "gauge_name", "Revolver.spin" ], level=level)

  let multiGaugeMessage level =
    let resource = [
      "host", "db-001"
      "service", "api-web"
      "measurement", "Processor.% Idle"
    ]

    Model.GaugeMessage(Gauge (Value.Fraction(83L, 100L), U.Percent),
                       Map resource,
                       Map [
                         "Core 1", (Gauge (Value.Fraction (1L, 1000L), U.Percent))
                         "Core 2", (Gauge (Value.Float 0.99, U.Percent))
                         "Core 3", (Gauge (Value.Float 0.473223755, U.Percent))
                       ],
                       level=level)

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
    let piUSD = Some (money Currency.USD 3.1415)
    let m = Model.Event("Unhandled exception", piUSD, name=PointName.parse "Logary.Tests")
    m.addExn(raisedExn "The cats are loose")
    m.setContextValues(Map [
      "user.name", Value.Str "haf"
      "user.id", Value.Str "deadbeef234567"
    ])
    m

  let helloWorld level =
    Model.Event("Hello World!", level=level, timestamp=0L)
