module Logary.Tests.Internals

open System.Collections.Generic
open Expecto
open Hopac
open Logary.Internals

[<Tests>]
let maps =
  testList "Map.ofObject" [
    testCase "converts primitive to empty map" <| fun _ ->
      let subject = Map.ofObject 3
      Expect.equal subject Map.empty "Should be empty map."

    testProperty "passes Map<string,obj> through unchanged" <| fun (extra : Map<string, obj>) ->
      let data = [ "a", [ "b", box 3; "c", box extra ] |> Map.ofList |> box ]
                 |> Map.ofList
                 |> box
      let subject = Map.ofObject data
      Expect.equal subject (data :?> Map<string, obj>) "Should equal input."

    testCase "converts null to empty map" <| fun _ ->
      let subject = Map.ofObject Unchecked.defaultof<obj>
      Expect.equal subject Map.empty "Should be empty map."

    testProperty "given int keys" <| fun (map : Map<int, string>) ->
      let subject : Map<string, obj> = Map.ofObject (Dictionary<int, string>(map))
      Expect.equal (subject |> Map.fold (fun acc k v -> acc |> Map.add (int k) (v :?> string)) Map.empty)
                   map
                   "should contain mapped ints"

    testCase "converts System.Collections.Generic.Dictionary<,> to map with equivalent keys" <| fun _ ->
      let toDic l = (Dictionary<_, _>(l |> Map.ofList))
      let subject = Map.ofObject (toDic ["a", box 4; "b", box 4M])
      Expect.equal subject
                   (["a", box 4; "b", box 4M] |> Map.ofList)
                   "Should equal map."

    testCase "given repeated keys" <| fun _ ->
      let raw   = [ KeyValuePair<_, _>(4, "hi there" |> box)
                    KeyValuePair<_, _>(4, "second value" |> box) ]
      let input = raw :> seq<KeyValuePair<_, _>>
      let subject = Map.ofObject (box input)
      Expect.equal (unbox subject.["4"]) "second value" "should have second item"

    testProperty "given list<KVP<string, string>>" <| fun kvs ->
      let raw : KeyValuePair<string, string> list = kvs
      let input = raw :> seq<KeyValuePair<_, _>>
      Map.ofObject (box input) |> ignore

    testProperty "given array<KVP<string, string>>" <| fun kvs ->
      let raw : KeyValuePair<string, string> array = kvs
      let input = raw :> seq<KeyValuePair<_, _>>
      Map.ofObject (box input) |> ignore

    testProperty "given seq<string * string>" <| fun kvs ->
      let raw : (string * string) list = kvs
      let input = raw :> seq<string * string>
      Map.ofObject (box input) |> ignore
    ]


[<Tests>]
let useStatementInLoops =
  let createCounter (increment : unit -> unit) =
    { new System.IDisposable with
        member x.Dispose () =
          increment () }

  let puree loops =
    let disposeCount = ref 0u
    let counter = createCounter (fun () -> disposeCount := !disposeCount + 1u)
    let rec inner (i : uint32) =
      if i = 0u then !disposeCount
      else
        use d = counter
        inner (i - 1u)
    inner loops

  let jobs loops =
    let disposeCount = ref 0u
    let counter = createCounter (fun () -> disposeCount := !disposeCount + 1u)
    let rec inner (i : uint32) : Job<uint32> =
      job {
        if i = 0u then return !disposeCount
        else
          use! x = Job.result counter
          return! inner (i - 1u)
      }
    inner loops

  let extracted loops =
    let g (x : Job<_>) =
      job {
        use! y = x
        return ()
      }
    let disposeCount = ref 0u
    let counter = createCounter (fun () -> disposeCount := !disposeCount + 1u)
    let rec inner (i : uint32) : Job<uint32> =
      job {
        if i = 0u then return !disposeCount
        else
          let! x = g (Job.result counter)
          return! inner (i - 1u)
      }
    inner loops

  testList "disposing with tail recursion" [
    // https://stackoverflow.com/questions/13491768/tail-recursion-and-exceptions-in-f
    // https://github.com/logary/logary/issues/216

    testCase "testing disposing with pure function" <| fun _ ->
      let loops = 10000u
      let disposed = puree loops
      Expect.equal disposed 0u "Showcases bad behaviour when the use-statement precludes tail-recursion"

    testCase "testing disposing with job function" <| fun _ ->
      let loops = 10000u
      let disposed = jobs loops |> run
      Expect.equal disposed 0u "Showcases bad behaviour when the use-statement precludes tail-recursion"

    testCase "testing disposing with job function extracted" <| fun _ ->
      let loops = 10000u
      let disposed = extracted loops |> run
      Expect.equal disposed loops "Should dispose the same number of times as there are loops"
  ]