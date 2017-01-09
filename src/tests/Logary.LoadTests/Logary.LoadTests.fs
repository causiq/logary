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
    for i in 0 .. 10000 do
      let (NonEmptyString name, testId : int64) = testId ()

      yield testCaseAsync (sprintf "getLogger with name (%s.%i)" name testId) ((fun () ->
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

      yield testCaseAsync (sprintf "Logger.getLoggerByName (%s.%i)" name testId) ((fun () ->
        withRegistry name testId <| fun (logary, out) ->
          let pname = PointName.ofSingle (sprintf "%s.%i" name testId)
          let msg = sprintf "Should have correct name: %O" pname
          let logged = sprintf "From %s.%i" name testId
          let logger = Logging.getLoggerByPointName pname
          job {
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