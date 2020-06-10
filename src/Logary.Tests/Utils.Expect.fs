namespace Logary.Tests

open System
open Expecto
open Expecto.Flip

module Expect =
  let private trim (s: string) = if isNull s then s else s.Trim()

  /// Expect (Result.Ok x) and return x, otherwise fail the test.
  let isOkX (m: string) (xR: Result<_,_>) =
    match xR with
    | Result.Ok x ->
      x
    | Result.Error e ->
      failtestf "Expected (Ok _), but got (Error %A). %s" e m

  /// Expect the passed float to be a number.
  let isNotNaN f format =
    if Double.IsNaN f then failtestf "%s. Float was the NaN (not a number) value." format

  /// Expect the passed float not to be positive infinity.
  let isNotPositiveInfinity actual format =
    if Double.IsPositiveInfinity actual then failtestf "%s. Float was positive infinity." format

  /// Expect the passed float not to be negative infinity.
  let isNotNegativeInfinity actual format =
    if Double.IsNegativeInfinity actual then failtestf "%s. Float was negative infinity." format

  /// Expect the passed float not to be infinity.
  let isNotInfinity actual format =
    isNotNegativeInfinity actual format
    isNotPositiveInfinity actual format
    // passed via excluded middle

  /// Expect the passed string not to be empty.
  let isNotEmpty (actual: string) format =
    Expect.isNotNull actual format
    if actual.Length = 0 then Tests.failtestf "%s. Should not be empty." format

  let linesEqual (message: string) (expected: string) (actual: string) =
    let sra = new IO.StringReader(actual)
    let sre = new IO.StringReader(expected)
    let mutable cont = true
    let mutable linea = null
    let mutable linee = null
    while cont do
      linea <- trim (sra.ReadLine())
      linee <- trim (sre.ReadLine())
      linea |> Expect.equal message linee
      cont <- not (isNull linea || isNull linee)

  let printSeq (xs: #seq<_>): string =
    xs |> Seq.mapi (fun i x -> sprintf "  [%i] %A" i x) |> String.concat Environment.NewLine

  /// This will pass:
  ///
  /// [ 1; 2; 3; 4; 5; 6 ]
  ///   |> Expect.sequenceContainsOrder "Valid ordering of subsequence" [ 1; 3; 5 ]
  ///
  /// This will fail:
  /// [ 1; 2; 3; 4; 5; 6 ]
  ///   |> Expect.sequenceContainsOrder "Wrong order of 0th and 1th elem" [ 3; 1; 6 ]
  ///
  /// This will fail:
  /// [ 1; 2; 3; 4; 5; 6 ]
  ///   |> Expect.sequenceContainsOrder "Missing 7 from actual" [ 1; 3; 7 ]
  ///
  /// This will pass:
  /// [ 1; 2; 3; 4; 5; 6 ]
  ///   |> Expect.sequenceContainsOrder "Empty list passes" []
  ///
  let sequenceContainsOrder message (expectedSub: #seq<'t>) (actual: #seq<'t>) =
    use ee = expectedSub.GetEnumerator()
    let el = System.Collections.Generic.Queue<'t> expectedSub
    use ae = actual.GetEnumerator()
    let al = ResizeArray<'t>()
    let nl = Environment.NewLine

    let rec iter i =
      if el.Count = 0 then (* success *) () else
      if not (ae.MoveNext()) then
        failtestf
          "Remainder of expected enumerable:%s%s%sWent through actual enumerable (%i items):%s%s"
          nl (printSeq el) nl i nl (printSeq al)
      else
        al.Add ae.Current
        let expected = el.Peek()
        if expected = ae.Current then
          ignore (el.Dequeue())
          iter (i + 1)
        else
          iter (i + 1)

    iter 0

  module Json =
    open Logary.Internals.Chiron

    /// Assert and pass through the value.
    let isObjectX message (value: Json) =
      match value with
      | Json.Object nested ->
        nested
      | other ->
        failtestf "%s. Expected Json.Object, but was %A" message other

    /// Assert the Json value is a Json.Object.
    let isObject message value =
      isObjectX message value |> ignore

    let isStringX message (value: Json) =
      match value with
      | String inner ->
        inner
      | other ->
        failtestf "%s Expected Json.String, but was %A" message other

    /// Assert and pass through the found field.
    let hasFieldX message field (value: JsonObject) =
      value
        |> JsonObject.toMap
        |> Map.tryFind field
        |> function
        | None ->
          failtestf "%s. Did not find field '%s' on Json.Object (%A)" message field value
        | Some f ->
          f

    /// Assert and pass through the JsonObject value.
    let hasFieldXX message field (value: JsonObject) =
      hasFieldX message field value |> ignore
      value

    /// Assert and pass through the JsonObject value.
    let hasFieldXXf message field callback (value: JsonObject) =
      callback (hasFieldX message field value) |> ignore
      value

    /// Assert the object has a field of the given name.
    let hasField message field value =
      hasFieldX message field value |> ignore
