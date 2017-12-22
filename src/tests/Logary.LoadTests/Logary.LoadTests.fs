module Logary.LoadTests

open System.Text
open System.IO
open Logary
open Logary.Message
open Logary.Targets
open Logary.Formatting
open Logary.Configuration
open Hopac
open Expecto
open Expecto.ExpectoFsCheck
open FsCheck
open Logary.Internals
open Logary.EventsProcessing
open NodaTime

let withRegistry name testId disableGlobals contFn =
  let textWriter () =
    let sb = new StringBuilder()
    let sw = new StringWriter(sb)
    sw.NewLine <- "\n"
    sw

  let out = textWriter ()

  let formatter =
    { new MessageWriter with
        member x.write tw m =
            sprintf "%O|%s" m.name m.value |> tw.WriteLine
    }

  let twTarget = TextWriter.create (TextWriter.TextWriterConf.create(out, out, formatter)) "tw"
  Config.create (sprintf "logary for %s.%i" name testId) "localhost"
  |> Config.target twTarget
  |> Config.ilogger (ILogger.LiterateConsole Error)
  |> Config.processing (Events.events |> Events.sink ["tw"])
  |> fun config -> if disableGlobals then config |> Config.disableGlobals else config
  |> Config.build
  |> Job.bind (fun logm ->
     let timeout = Duration.FromSeconds(5L)
     let dispose = logm.shutdown () :> Job<unit>
     Job.tryFinallyJobDelay (fun _ -> contFn (logm, out)) dispose
  )

let withRegistryAsync name testId disableGlobals contFn =
  withRegistry name testId disableGlobals contFn
  |> Job.toAsync

[<Tests>]
let tests =
  let gen =
    gen { let! name = Arb.generate<NonEmptyString>
          let! testId = Arb.generate<int64>
          return name, testId }

  let testId () =
    gen
    |> Gen.sample 10 1
    |> List.head

  testList "registry" [

    // yield memoized
    yield testSequenced (
      testList "globals globals" [
        testCaseAsync "async single flyweight - single registry" ((fun () ->
          withRegistryAsync "A.B.C and D.E" 123456L false <| fun (logary, out) ->
            job {
              let abc = Logging.getLoggerByName "A.B.C"
              let de = Logging.getLoggerByName "D.E"
              do! abc.debugWithAck (eventX "Hi there")
              do! de.debugWithAck (eventX "Goodbye")
              Expect.equal (out.ToString()) "A.B.C|Hi there\nD.E|Goodbye\n"
                           "Should have correct output"
          })())

        testCaseAsync "async single flyweight - static getLoggerByName - multi registry" ((fun () ->
          let interfere = withRegistry "interference" 654321L false <| fun (logary, out) -> Job.result (out)
          let abc = Logging.getLoggerByName "A.B.C"
          let de = Logging.getLoggerByName "D.E"

          let first = withRegistry "A.B.C and D.E" 123456L false <| fun (logary, out) ->
            job {
              do! abc.debugWithAck (eventX "Hi there")
              do! de.debugWithAck (eventX "Goodbye")
              // Expect.equal (out.ToString()) "A.B.C|Before\nA.B.C|Hi there\nD.E|Goodbye\n"
              Expect.equal (out.ToString()) "A.B.C|Hi there\nD.E|Goodbye\n"
                          "Should have correct output"
            }

          interfere
          |> Job.bind (fun out ->
            printfn "Before starting new..."
            // interfere should be shutdown at this point
            abc.debugWithAck (eventX "Before")
            |> Job.map (fun _ ->
               Expect.equal (out.ToString()) "" "should have no output, since logary has been shutdown"
              //  Expect.isNone (!Internals.Globals.singleton) "Should have no value"
               )
          )
          |> Job.bind (fun _ -> first)
          |> Job.toAsync
          )())
      ]
    )

    for i in 0 .. 10000 do
      let (NonEmptyString name, testId : int64) = testId ()

      yield testCaseAsync (sprintf "async many ready - instance getLogger - (%s.%i) - %s" name testId (System.Guid.NewGuid().ToString("N"))) ((fun () ->
        withRegistryAsync name testId true <| fun (logary, out) ->
          let pname = PointName.ofSingle (sprintf "%s.%i" name testId)
          let msg = sprintf "Should have correct name: %O" pname
          let logged = sprintf "From %s.%i" name testId
          job {
            let logger = logary.getLogger pname
            Expect.equal logger.name pname msg
            do! logger.infoWithAck (eventX logged)
            let! finfo = logary.flushPending (Duration.FromMilliseconds 500L)
            Expect.equal (out.ToString()) (sprintf "%O|%s\n" pname logged) "Should log right value"
          }
      ) ())

      yield testCaseAsync (sprintf "async many ready - static getLoggerByName - (%s.%i) - %s" name testId (System.Guid.NewGuid().ToString("N")) ) ((fun () ->
        withRegistryAsync name testId true <| fun (logary, out) ->
          let pname = PointName.ofSingle (sprintf "%s.%i" name testId)
          let msg = sprintf "Should have correct name: %O" pname
          let logged = sprintf "From %s.%i" name testId
          job {
            // static get logger means always use the latest registry ('logary instance')
            // so, we can not guarantee the output are in each registry's target output.
            let logger = Logging.getLoggerByPointName pname
            Expect.equal logger.name pname msg
            // do! logger.infoWithAck (eventX logged)
            // let! finfo = logary.flushPending (Duration.FromMilliseconds 500L)
            // Expect.equal (out.ToString()) (sprintf "%O|%s\n" pname logged) "Should log right value"
          }
      ) ())
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv