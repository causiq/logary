module FsMtParserFull

open System.Text

type CaptureHint = Default = 0 | Structure = 1 | Stringify = 2
type AlignDirection = Right = 0 | Left = 1

[<Struct>]
type AlignInfo =
    new (direction : AlignDirection, width : int) = { _direction=direction; _width=width; }
    new (isValid : bool) = { _direction = AlignDirection.Right; _width = (if isValid then -1 else -2) }
    val private _direction : AlignDirection
    val private _width : int
    member this.direction with get() = this._direction
    member this.width with get() = this._width
    member this.isEmpty = this.width = -1
    member internal this.isValid = this.width <> -2
    static member empty = AlignInfo(isValid=true)
    static member invalid = AlignInfo(isValid=false)

type Property(name : string, format : string, captureHint : CaptureHint, align : AlignInfo) =
  static let emptyInstance = Property("", null, CaptureHint.Default, AlignInfo.empty)
  static member empty = emptyInstance
  member x.name = name
  member x.format = format
  member x.captureHint = captureHint
  member x.align = align
  member internal x.AppendPropertyString(sb : StringBuilder, ?replacementName) =
    sb.Append("{")
      .Append(match x.captureHint with CaptureHint.Structure -> "@" | CaptureHint.Stringify -> "$" | _ -> "")
      .Append(defaultArg replacementName name)
      .Append(match x.format with null | "" -> "" | _ -> ":" + x.format) |> ignore

    if not (x.align.isEmpty) then
      sb.Append(if x.align.direction = AlignDirection.Right then ",-" else ",")
        .Append(string x.align.width) |> ignore

    sb.Append("}")
  override x.ToString() = x.AppendPropertyString(StringBuilder()).ToString()

module internal ParserBits =

  let inline isLetterOrDigit c = System.Char.IsLetterOrDigit c
  let inline isValidInPropName c = c = '_' || System.Char.IsLetterOrDigit c
  let inline isValidInAlignment c = c = '-' || System.Char.IsDigit c
  let inline isValidInCaptureHint c = c = '@' || c = '$'
  let inline isValidInFormat c = c <> '}' && (c = ' ' || isLetterOrDigit c || System.Char.IsPunctuation c)
  let inline isValidCharInPropTag c = c = ':' || isValidInPropName c || isValidInFormat c || isValidInCaptureHint c

  [<Struct>]
  type Range(startIndex : int, endIndex : int) =
    member inline x.start = startIndex
    member inline x.``end`` = endIndex
    member inline x.length = (endIndex - startIndex) + 1
    member inline x.getSubstring (s : string) = s.Substring(startIndex, x.length)
    member inline x.isEmpty = startIndex = -1 && endIndex = -1
    override x.ToString() = sprintf "(start=%i, end=%i)" x.start x.``end``
    static member inline substring (s : string, startIndex, endIndex) = s.Substring(startIndex, (endIndex - startIndex) + 1)
    static member inline empty = Range(-1, -1)

  let inline tryGetFirstCharInRange predicate (s : string) (range : Range) =
    let rec go i =
      if i > range.``end`` then -1
      else if not (predicate s.[i]) then go (i+1) else i
    go range.start

  let inline tryGetFirstChar predicate (s : string) first =
    tryGetFirstCharInRange predicate s (Range(first, s.Length - 1))

  let inline hasAnyInRange predicate (s : string) (range : Range) =
    match tryGetFirstChar (predicate) s range.start with
    | -1 ->
      false
    | i ->
      i <= range.``end``

  let inline hasAny predicate (s : string) = hasAnyInRange predicate s (Range(0, s.Length - 1))
  let inline indexOfInRange s range c = tryGetFirstCharInRange ((=) c) s range

  /// Attemps to parse an integer from the range within a string. Returns invalidValue if the
  /// string does not contain an integer. The '-' is allowed only as the first character.
  let inline tryParseIntFromRng (invalidValue : int) (s : string) (range : Range) =
    if range.length = 1 && '0' <= s.[0] && s.[0] <= '9' then
      int (s.[range.start]) - 48
    else
      let indexOfLastCharPlus1 = range.``end``+1
      let rec inside isNeg numSoFar i =
        if i = indexOfLastCharPlus1 then
          if isNeg then -numSoFar else numSoFar
        else
          let c = s.[i]
          if c = '-' then
            if i = range.start then inside true (numSoFar) (i+1)
            else invalidValue // no '-' character allowed other than first char
          elif '0' <= c && c <= '9' then
            inside isNeg (10*numSoFar + int c - 48) (i+1)
          else invalidValue

      inside false 0 range.start

  /// Will parse ",-10"   as AlignInfo(direction=Left, width=10).
  /// Will parse ",5"     as AlignInfo(direction=Right, width=5).
  /// Will parse ",0"     as AlignInfo(isValid=false) because 0 is not a valid alignment.
  /// Will parse ",-0"    as AlignInfo(isValid=false) because 0 is not a valid alignment.
  /// Will parse ",,5"    as AlignInfo(isValid=false) because ',5' is not an int.
  /// Will parse ",5-"    as AlignInfo(isValid=false) because '-' is in the wrong place.
  /// Will parse ",asdf"  as AlignInfo(isValid=false) because 'asdf' is not an int.
  let inline tryParseAlignInfoRng (s:string) (rng:Range) =
    match s, rng with
    | s, rng when (rng.start > rng.``end``) || (hasAnyInRange (not << isValidInAlignment) s rng) ->
      AlignInfo.invalid

    | s, rng ->
      let width =
        match tryParseIntFromRng (System.Int32.MinValue) s rng with
        | System.Int32.MinValue -> 0 // not a valid align number (e.g. dash in wrong spot)
        | n -> n

      if width = 0 then AlignInfo.invalid
      else
        let isNegativeAlignWidth = width < 0
        let direction = if isNegativeAlignWidth then AlignDirection.Left else AlignDirection.Right
        AlignInfo(direction, abs(width))

  /// Attempts to validate and parse a property token within the specified range. If the property
  /// insides contains any invalid characters, then the `Property.empty' instance is returned.
  let inline tryGetPropInRange (template : string) (within : Range) : Property =
    let nameRange, alignRange, formatRange =
      match indexOfInRange template within ',', indexOfInRange template within ':' with
      | -1, -1 ->
        // neither align nor format
        within, Range.empty, Range.empty

      | -1, fmtIdx ->
        // has format part, but does not have align part
        Range(within.start, fmtIdx-1), Range.empty, Range(fmtIdx+1, within.``end``)

      | alIdx, -1 ->
        // has align part, but does not have format part
        Range(within.start, alIdx-1), Range(alIdx+1, within.``end``), Range.empty

      | alIdx, fmtIdx when alIdx < fmtIdx && alIdx <> (fmtIdx-1) ->
        // has both align and format parts, in the correct order
        let align = Range(alIdx+1, fmtIdx-1)
        let fmt = Range(fmtIdx+1, within.``end``)
        Range(within.start, alIdx-1), align, fmt

      | alIdx, fmtIdx when alIdx > fmtIdx ->
        // has format part, no align (but one or more commas *inside* the format string)
        Range(within.start, fmtIdx-1), Range.empty, Range(fmtIdx+1, within.``end``)

      | _, _ ->
        Range.empty, Range.empty, Range.empty

    if nameRange.isEmpty then
      Property.empty // property name is empty
    else
      let maybeCaptureHintChar = template.[nameRange.start]
      let propertyNameStartIndex, captureHint =
        match maybeCaptureHintChar with
        | '@' -> nameRange.start+1, CaptureHint.Structure
        | '$' -> nameRange.start+1, CaptureHint.Stringify
        | _ -> nameRange.start, CaptureHint.Default

      let propertyName = Range.substring (template, propertyNameStartIndex, nameRange.``end``)
      if propertyName = "" || (hasAny (not<<isValidInPropName) propertyName) then
        // property name has invalid characters
        Property.empty
      else
        if (not formatRange.isEmpty) && (hasAnyInRange (not<<isValidInFormat) template formatRange) then
          Property.empty // format range has invalid characters
        else
          match alignRange.isEmpty, formatRange.isEmpty with
          | true, true -> Property(propertyName, null, captureHint, AlignInfo.empty)
          | true, false -> Property(propertyName, formatRange.getSubstring template, captureHint, AlignInfo.empty)
          | false, _ ->
            let formatString = if formatRange.isEmpty then null else formatRange.getSubstring template
            match tryParseAlignInfoRng template alignRange with
            | ai when ai.isValid -> Property(propertyName, formatString, captureHint, ai)
            | _ -> Property.empty // align has invalid characters

  let inline findNextNonPropText (startAt : int) (template : string) (foundText : string->unit) : int =
    // Finds the next text token (starting from the 'startAt' index) and returns the next character
    // index within the template string. If the end of the template string is reached, or the start
    // of a property token is found (i.e. a single { character), then the 'consumed' text is passed
    // to the 'foundText' method, and index of the next character is returned.
    let mutable escapedBuilder = Unchecked.defaultof<StringBuilder> // don't create one until it's needed
    let inline append (ch : char) = if not (isNull escapedBuilder) then escapedBuilder.Append(ch) |> ignore
    let inline createStringBuilderAndPopulate i =
      if isNull escapedBuilder then
        escapedBuilder <- StringBuilder() // found escaped open-brace, take the slow path
        for chIndex = startAt to i-1 do append template.[chIndex] // append all existing chars
    let rec go i =
      if i >= template.Length then
        template.Length // bail out at the end of the string
      else
        let ch = template.[i]
        match ch with
        | '{' ->
          if (i+1) < template.Length && template.[i+1] = '{' then
            createStringBuilderAndPopulate i
            append ch; go (i+2)
          else i // found an open brace (potentially a property), so bail out
        | '}' when (i+1) < template.Length && template.[i+1] = '}' ->
          createStringBuilderAndPopulate i
          append ch; go (i+2)
        | _ ->
          append ch; go (i+1)

    let nextIndex = go startAt
    if (nextIndex > startAt) then // if we 'consumed' any characters, signal that we 'foundText'
      if isNull escapedBuilder then
        foundText (Range.substring(template, startAt, nextIndex - 1))
      else
        foundText (escapedBuilder.ToString())
    nextIndex

  let findPropOrText (start : int) (template : string)
                      (foundText : string -> unit)
                      (foundProp : Property -> unit) : int =
    // Attempts to find the indices of the next property in the template
    // string (starting from the 'start' index). Once the start and end of
    // the property token is known, it will be further validated (by the
    // tryGetPropInRange method). If the range turns out to be invalid, it's
    // not a property token, and we return it as text instead. We also need
    // to handle some special case here: if the end of the string is reached,
    // without finding the close brace (we just signal 'foundText' in that case).
    let nextInvalidCharIndex =
      match tryGetFirstChar (not << isValidCharInPropTag) template (start+1) with
      | -1 ->
        template.Length
      | idx ->
        idx

    if nextInvalidCharIndex = template.Length || template.[nextInvalidCharIndex] <> '}' then
      foundText (Range.substring(template, start, (nextInvalidCharIndex - 1)))
      nextInvalidCharIndex
    else
      let nextIndex = nextInvalidCharIndex + 1
      let propInsidesRng = Range(start + 1, nextIndex - 2)
      match tryGetPropInRange template propInsidesRng with
      | prop when not (obj.ReferenceEquals(prop, Property.empty)) ->
        foundProp prop
      | _ ->
        foundText (Range.substring(template, start, (nextIndex - 1)))
      nextIndex

/// Parses template strings such as "Hello, {PropertyWithFormat:##.##}"
/// and calls the 'foundTextF' or 'foundPropF' functions as the text or
/// property tokens are encountered.
let parseParts (template : string) foundTextF foundPropF =
  let tlen = template.Length
  let rec go start =
    if start >= tlen then ()
    else match ParserBits.findNextNonPropText start template foundTextF with
          | next when next <> start ->
            go next
          | _ ->
            go (ParserBits.findPropOrText start template foundTextF foundPropF)
  go 0