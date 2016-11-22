module Logary.Tests.Value

open System
open System.Collections.Generic
open NodaTime
open Expecto
open Logary
open FsCheck

type NormalPerson =
  { name : string option }

type Shark =
  | Mako
  | Bull
  | GreatWhite

type StateOfMind =
  | Flummoxed
  | Sharp
  | Shark of Shark

type AtypicalPerson =
  { name : string
    mind : StateOfMind }

  // this causes the reflection to go bonkers:
  static member empty =
    { name = "haf"
      mind = Sharp }

type StrangePerson =
  { name : string
    description : string option
    age : uint16 option
    bicycles : uint32 option
    positivity : int64
    happiness : int32
    smirking : Nullable<int>
    nullString : string
    properties : Map<string, obj>
    birthday : DateTimeOffset }

  static member empty =
    { name = "haf"
      description = Some "evening time hacker"
      age = Some 92us
      bicycles = Some 3u
      positivity = Int64.MaxValue
      happiness = Int32.MaxValue
      smirking = Nullable<_>(-1)
      nullString = null
      properties =
        Map [
          "dude", box true
          "bro", box false
          "boxed", box "oh noes"
        ]
      birthday = DateTimeOffset(1970, 01, 01, 0, 0, 0, 0, TimeSpan.Zero)
    }

[<Tests>]
let valueTests =
  testList "value tests" [
    testCase "converting boxed int array" <| fun _ ->
      Expect.equal (Value.ofObject (box [| 1; 2; 3 |]))
                   (Array [ Int64 1L; Int64 2L; Int64 3L ])
                   "should convert boxed array of ints to Value.Array of Int64"

    testCase "converting null array" <| fun _ ->
      let arr = [|null|]
      Value.ofObject arr |> ignore

    testProperty "converting null IEnumerable<>" <| fun (aList : list<_>)  ->
      let a = aList :>  IEnumerable<obj>
      Value.ofObject a |> ignore

    testProperty "converting arbitrary array" <| fun (arr : _[]) ->
      Value.ofObject arr |> ignore

    testProperty "converting arbitrary object" <| fun (a : System.Object) ->
      Value.ofObject a |> ignore

    testProperty "converting arbitrary FSharp list" <| fun (a : list<_>) ->
      Value.ofObject a |> ignore

    testProperty "converting arbitrary System.Collections.Generic.List" <| fun (a : List<_>) ->
      Value.ofObject a |> ignore

    testProperty "converting arbitrary list" <| fun (a : list<KeyValuePair<string, obj>>) ->
      Value.ofObject a |> ignore

    testCase "convert DU with field" <| fun _ ->
      Expect.equal (Value.ofObject <| Shark Mako)
                   (Object (Map ["Shark", String "Mako"]))
                   "Unions should display their fields in the value"

    testCase "convert normal person" <| fun _ ->
      Value.ofObject { NormalPerson.name = Some "haf" } |> ignore

    testCase "convert a girl with no name" <| fun _ ->
      Value.ofObject { NormalPerson.name = None } |> ignore

    testCase "convert atypical person" <| fun _ ->
      Value.ofObject { name = "haf"; mind = Shark Mako } |> ignore

    testCase "convert atypical person with no name" <| fun _ ->
      Value.ofObject { name = null; mind = Shark Bull } |> ignore

    testCase "convert an empty but strange person" <| fun _ ->
      Value.ofObject StrangePerson.empty |> ignore

    testProperty "convert strange person" <| fun (p : StrangePerson) ->
      Value.ofObject p |> ignore
(*
value tests/converting a strange person: Exception: System.NullReferenceException: Object reference not set to an instance of an object
  at FsCheck.Res.exc (System.Exception e) <0x37b4b48 + 0x0001c> in <filename unknown>:0
  at FsCheck.Testable.evaluate[a,b] (Microsoft.FSharp.Core.FSharpFunc`2 body, FsCheck.a a) <0x35ab398 + 0x00177> in <filename unknown>:0
  at FsCheck.Testable+forAll@173-2[a,b].Invoke (a a') <0x35ab358 + 0x0002f> in <filename unknown>:0
  at FsCheck.Testable+props@153-2[a,c].Invoke (Microsoft.FSharp.Core.Unit unitVar) <0x34681d8 + 0x0001d> in <filename unknown>:0
  at Microsoft.FSharp.Control.LazyExtensions+Create@6241[T].Invoke () <0x344c518 + 0x0001b> in <filename unknown>:0
  at System.Lazy`1[T].CreateValue () <0x1799180 + 0x0018f> in <filename unknown>:0  (00:00:01.5435279)
*)
  ]

  
  
