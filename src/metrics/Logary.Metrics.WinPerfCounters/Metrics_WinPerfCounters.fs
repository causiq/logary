module Logary.Metrics.WinPerfCounters

open System.Diagnostics
open Hopac
open Logary
open Logary.Metric
open Logary.Internals
open Logary.Metrics.WinPerfCounter
open Logary.Metrics.AllWinPerfCounters

module Common =

  //let system =
  //  [ System.``Context Switches/sec``
  //    System.``Processor Queue Length`` ]

  //let dotnet instance =
  //  [ ``_NET CLR Exceptions``.``# of Exceps Thrown / sec`` instance ]

  let dotnet instance =
    [ ``_NET CLR Memory``.``Gen 0 Promoted Bytes/Sec`` instance
      ``_NET CLR Memory``.``Gen 1 Promoted Bytes/Sec`` instance
      ``_NET CLR Memory``.``# Bytes in all Heaps`` instance
    ]

  let cpuTime =
    [ Processor.``% Processor Time``
      Processor.``% User Time``
      Processor.``% Interrupt Time``
      Processor.``% Privileged Time``
      //Processor.``Interrupts/sec``
      //Processor.``% Idle Time`` 

    ]
    |> List.map (fun f -> f (Instance WinPerfCounter.KnownInstances._Total))

  let proc instance =
    [ Process.``% Processor Time`` instance
      Process.``% User Time`` instance
      Process.``% Privileged Time`` instance
      Process.``Virtual Bytes`` instance
    ]

  /// Useful ASP.Net counters, from
  /// http://technet.microsoft.com/en-us/library/cc778343%28v=ws.10%29.aspx
  ///
  /// You can either pass the app domain instance process name, or you could use
  /// WinPerfCounter.pidInstance () to get your own instance name. If you have
  /// multiple processes with the same name executing, you will need to
  /// discriminate by name.
  ///
  /// Use WinPerfCounter.pidInstance () to get the current process' instance.
  let recommended instance =
    cpuTime
    @ proc instance
    @ dotnet instance

  let recommendedProc () =
    let procInstance = pidInstance ()
    cpuTime
    @ proc procInstance
    @ dotnet procInstance

  /// A unified naming scheme for the names of performance counters
  module Naming =

    let toCounter (PointName dp) =
      match dp with
      | [ category; counter ] ->
        { category = category; counter = counter; instance = NotApplicable }

      | [ category; counter; instance ] ->
        { category = category; counter = counter; instance = Instance instance }

      | _ ->
        failwithf "unknown performance counter name: %A" dp

  let tryGetPc (lastValues : Map<PointName, PC * Value>) dp =
    lastValues
    |> Map.tryFind dp
    |> Option.map fst
    |> function
    | None -> toPC (Naming.toCounter dp)
    | x -> x

  let pcNextValue (pc : PC) =
    Float (nextValue pc)