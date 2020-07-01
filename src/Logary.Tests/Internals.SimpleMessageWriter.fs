module Logary.Tests.Internals.SimpleMessageWriter

open System.IO
open System.Text
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks
open Logary
open Logary.Internals
open Expecto
open Expecto.Flip

let sut = SimpleMessageWriter()

let givenContext () =
  let sb = StringBuilder()
  let tw = new StringWriter(sb)
  sb, tw

let runWriter (sut: MessageWriter) (ct: CancellationToken) (message: LogaryMessage) =
  task {
    let sb, tw = givenContext ()
    do! sut.write(tw, message, ct)
    return sb.ToString()
  }

let private createAsyncUnitWrapper (xCT2yT: _ -> Task) =
  async.Delay(fun () -> async.Bind(Async.CancellationToken, fun ct -> Async.AwaitTask (xCT2yT ct)))
let private createAsyncWrapper (xCT2yT: _ -> Task<'a>) =
  async.Delay(fun () -> async.Bind(Async.CancellationToken, fun ct -> Async.AwaitTask (xCT2yT ct)) |> Async.Ignore)

let testCaseUnitTask name (xCT2yT: CancellationToken -> Task) =
  testCaseAsync name (createAsyncUnitWrapper xCT2yT)
let ftestCaseUnitTask name (xCT2yT: CancellationToken -> Task) =
  ftestCaseAsync name (createAsyncUnitWrapper xCT2yT)
let ptestCaseUnitTask name (xCT2yT: CancellationToken -> Task) =
  ptestCaseAsync name (createAsyncUnitWrapper xCT2yT)

let testCaseTask name (xCT2yT: CancellationToken -> Task<'a>) =
  testCaseAsync name (createAsyncWrapper xCT2yT)
let ftestCaseTask name (xCT2yT: CancellationToken -> Task<'a>) =
  ftestCaseAsync name (createAsyncWrapper xCT2yT)
let ptestCaseTask name (xCT2yT: CancellationToken -> Task<'a>) =
  ptestCaseAsync name (createAsyncWrapper xCT2yT)

[<Tests>]
let tests =
  testList "simple message writer" [
    testList "event" [
      testCaseTask "simple" <| fun ct -> task {
        let! result = Model.Event("Hello world", Some <| money Currency.USD 45., level=Info) |> runWriter sut ct

        result
          |> Expect.isNotEmpty "Should have a value"

        result
          |> Expect.stringContains "Should contain the event message" "Hello world"

        result
          |> Expect.stringContains "Should contain the level of the event in lowercase characters" "info"
      }
    ]

    testList "span" [

    ]

    testList "gauge" [

    ]

    testList "histogram" [

    ]

    testList "identify user" [

    ]

    testList "set user property" [

    ]
  ]
  |> testLabel "logary"