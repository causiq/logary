module Integration

open Fuchu

[<Tests>]
let integration =
  testList "[integration] executing sql statements" [
    testCase "" <| fun _ ->
      ()
    ]