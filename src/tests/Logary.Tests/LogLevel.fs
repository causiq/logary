module Logary.Tests.LogLevel

open Expecto
open System
open Logary

[<Tests>]
let tests =
  testList "LogLevel" [
    yield testCase "can compare LogLevels" <| fun _ ->
      Expect.isLessThan Info Error "Info is a lesser level than Error"

    for i in 1..6 do
      let (l1, l2) = i, i
      let l1, l2 = LogLevel.ofInt l1, LogLevel.ofInt l2
      yield testCase "can equate LogLevel structural" <| fun _ ->
        Expect.equal l1 l2 "are equal"

      yield testCase "can equate LogLevel IComparable" <| fun _ ->
        Expect.equal ((l1 :> IComparable).CompareTo(l2)) 0 "Comparable equal"

      yield testCase "can equate LogLevel IComparable<LogLevel>" <| fun _ ->
        Expect.equal ((l1 :> IComparable<LogLevel>).CompareTo(l2)) 0 "CompareTo equal"

    for i in 1..5 do
      let (l1, l2) = i, (i + 1)
      let l1, l2 = LogLevel.ofInt l1, LogLevel.ofInt l2
      yield testCase "can compare LogLevel less structural" <| fun _ ->
        Expect.isLessThan l1 l2 "should be less"

      yield testCase "can compare LogLevel less IComparable" <| fun _ ->
        Expect.equal ((l1 :> IComparable).CompareTo(l2)) -1 "compare negative -1"
      yield testCase "can compare LogLevel less IComparable<LogLevel>" <| fun _ ->
        Expect.equal ((l1 :> IComparable<LogLevel>).CompareTo(l2)) -1 "compare negative 1"

    ]
