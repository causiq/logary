module Logary.Tests.LogLevel

open Fuchu
open Swensen.Unquote

open System

open Logary

[<Tests>]
let tests =
  testList "LogLevel" [    
    yield testCase "can compare LogLevels" <| fun _ ->
      Info <? Error
      Error >? Info

    for i in 1..6 do
      let (l1, l2) = i, i
      let l1, l2 = LogLevel.FromInt l1, LogLevel.FromInt l2
      yield testCase "can equate LogLevel structural" <| fun _ ->
        l1 =? l2
      yield testCase "can equate LogLevel IComparable" <| fun _ ->
        (l1 :> IComparable).CompareTo(l2) =? 0
      yield testCase "can equate LogLevel IComparable<LogLevel>" <| fun _ ->
        (l1 :> IComparable<LogLevel>).CompareTo(l2) =? 0

    for i in 1..5 do
      let (l1, l2) = i, (i + 1)
      let l1, l2 = LogLevel.FromInt l1, LogLevel.FromInt l2
      yield testCase "can compare LogLevel less structural" <| fun _ ->
        l1 <? l2
      yield testCase "can compare LogLevel less IComparable" <| fun _ ->
        (l1 :> IComparable).CompareTo(l2) =? -1
      yield testCase "can compare LogLevel less IComparable<LogLevel>" <| fun _ ->
        (l1 :> IComparable<LogLevel>).CompareTo(l2) =? -1

    ]