module Fuchu

open Fuchu

type Assert =
  /// specify two floats equal within a given error - epsilon.
  static member FloatEqual(msg, expected, actual, ?epsilon) =
    let epsilon = defaultArg epsilon 0.001
    if expected <= actual + epsilon && expected >= actual - epsilon then
      ()
    else
      Tests.failtestf "Expected %f to be %f within %f epsilon. %s"
        actual expected epsilon msg
