module TestHelpers

open Expecto

let stringEqual (actual: 'a) (expected: 'a) (msg: string) =
  if expected = actual then ()
  else Tests.failtestf "Expected %A \r\nto equal %A. \r\n %s"
                       actual expected msg
