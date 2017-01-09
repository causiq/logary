module Logary.LoadTests

open System.Text
open System.IO
open Logary
open Logary.Targets
open Logary.Configuration
open Hopac
open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

let withRegistry name testId contFn =
  let textWriter () =
    let sb = new StringBuilder()
    new StringWriter(sb)

  let out, err = textWriter (), textWriter ()

  let target =
    Target.confTarget name (
      TextWriter.create (TextWriter.TextWriterConf.create(out, err))
    )

  confLogary (sprintf "logary for %s.%i" name testId)
  |> withRule (Rule.createForTarget name)
  |> withTarget target
  |> Config.validate
  |> runLogary
  |> Job.bind contFn
  |> Job.toAsync

[<Tests>]
let tests =

  let testId () =
    gen { let! name = Arb.generate<NonEmptyString>
          let! testId = Arb.generate<int64>
          return name, testId }
    |> Gen.sample 0 1
    |> List.head

  testList "load tests" [
    for i in 0 .. 10000 do
      yield testCaseAsync "logger name" ((fun () ->
        let (NonEmptyString name, testId : int64) = testId ()
        withRegistry name testId <| fun logary ->
          let pname = PointName.ofSingle (sprintf "%s.%i" name testId)
          job {
            let! logger = Registry.getLogger logary.registry pname
            let msg = sprintf "Should have correct name: %O" pname
            Expect.equal logger.name pname msg
          }
      ) ())
  ]

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv