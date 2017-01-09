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

let withRegistry name testId contFn =
  let textWriter () =
    let sb = new StringBuilder()
    new StringWriter(sb)

  let out = textWriter ()

  let formatter =
    { new StringFormatter with
        member x.format m =
          match m.value with
          | Event format ->
            sprintf "%O|%s" m.name format
          | _ ->
            failwith "not supported"
    }

  let target =
    Target.confTarget name (
      TextWriter.create (
        TextWriter.TextWriterConf.create(out, out, formatter)
      )
    )

  confLogary (sprintf "logary for %s.%i" name testId)
  |> withRule (Rule.createForTarget name)
  |> withTarget target
  |> Config.validate
  |> runLogary
  |> Job.bind (fun logary ->
    let dispose = Config.shutdownSimple logary |> Job.Ignore
    Job.tryFinallyJobDelay (fun _ -> contFn (logary, out)) dispose
  )
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
          withRegistry "A.B.C and D.E" 123456L <| fun (logary, out) ->
            job {
              let abc = Logging.getLoggerByName "A.B.C"
              let de = Logging.getLoggerByName "D.E"
              do! abc.debugWithAck (eventX "Hi there")
              do! de.debugWithAck (eventX "Goodbye")
              Expect.equal (out.ToString()) "A.B.C|Hi there\nD.E|Goodbye\n"
                           "Should have correct output"
          })())

        testCaseAsync "async single flyweight - static getLoggerByName - multi registry" ((fun () ->
          let interfere = withRegistry "interference" 654321L <| fun (logary, out) -> Job.result ()

          let abc = Logging.getLoggerByName "A.B.C"
          let de = Logging.getLoggerByName "D.E"
          let first = withRegistry "A.B.C and D.E" 123456L <| fun (logary, out) ->
            job {
              do! abc.debugWithAck (eventX "Hi there")
              do! de.debugWithAck (eventX "Goodbye")
              Expect.equal (out.ToString()) "A.B.C|Before\nA.B.C|Hi there\nD.E|Goodbye\n"
                          "Should have correct output"
            }

          interfere
          |> Async.map (fun _ ->
            // interfere should be shutdown at this point
            abc.debugWithAck (eventX "Before") |> start
            Expect.isNone (!Internals.Globals.singleton) "Should have no value"
          )
          |> Async.bind (fun _ -> first)
          )())
      ]
    )

    for i in 0 .. 10000 do
      let (NonEmptyString name, testId : int64) = testId ()

      yield testCaseAsync (sprintf "async many ready - instance getLogger - (%s.%i)" name testId) ((fun () ->
        withRegistry name testId <| fun (logary, out) ->
          let pname = PointName.ofSingle (sprintf "%s.%i" name testId)
          let msg = sprintf "Should have correct name: %O" pname
          let logged = sprintf "From %s.%i" name testId
          job {
            let! logger = Registry.getLogger logary.registry pname
            Expect.equal logger.name pname msg
            do! logger.infoWithAck (eventX logged)
            do! Registry.Advanced.flushPending logary.registry
            Expect.equal (out.ToString()) (sprintf "%O|%s\n" pname logged) "Should log right value"
          }
      ) ())

      yield testCaseAsync (sprintf "async many ready - static getLoggerByName - (%s.%i)" name testId) ((fun () ->
        withRegistry name testId <| fun (logary, out) ->
          let pname = PointName.ofSingle (sprintf "%s.%i" name testId)
          let msg = sprintf "Should have correct name: %O" pname
          let logged = sprintf "From %s.%i" name testId
          job {
            let logger = Logging.getLoggerByPointName pname
            Expect.equal logger.name pname msg
            do! logger.infoWithAck (eventX logged)
            do! Registry.Advanced.flushPending logary.registry
            Expect.equal (out.ToString()) (sprintf "%O|%s\n" pname logged) "Should log right value"
          }
      ) ())
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv