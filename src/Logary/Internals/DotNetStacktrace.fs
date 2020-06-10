module Logary.Internals.DotNetStacktrace

open Logary
open System
open System.Text.RegularExpressions

let internal mline = Regex(@"at (?<site>.+?)(( in )((?<file>[^\n]+?)(:line (?<lineNo>\d{1,}))?))?$", RegexOptions.Multiline ||| RegexOptions.IgnoreCase)
let internal mdelim = Regex(@"\S*--- [a-zA-Z ]+---", RegexOptions.Multiline ||| RegexOptions.IgnoreCase)
let internal mexnt = Regex(@"^(?<exnt>[a-z0-9\.+`]+Exception([a-z0-9`\.+\]\[]+)?): (?<msg>.+)$", RegexOptions.Multiline ||| RegexOptions.IgnoreCase)

type internal StacktraceLine =
  /// System.IO.FileNotFoundException: Could not load file or assembly 'Google.Api.Gax.Rest, Version=2.2.1.0, Culture=neutral, PublicKeyToken=3ec5ea7f18953e47' or one of its dependencies. The system cannot find the file specified.
  | ExnType of exnType: string * message: string
  /// "    at ServiceStack.ServiceClient.Web.ServiceClientBase.Send[TResponse](String httpMethod, String relativeOrAbsoluteUrl, Object request)"
  | Frame of frame: StackFrame
  /// "--- End of inner exception stack trace ---"
  | Delim
  /// WRN: Assembly binding logging is turned OFF.
  | Text of data:string

let internal (|Match|NoMatch|) (line: string) =
  let mline = mline.Match line
  let mdelim = mdelim.Match line
  if mline.Success then
    let site = mline.Groups.["site"].Value
    let file = let f = mline.Groups.["file"] in if not f.Success || String.IsNullOrWhiteSpace f.Value then None else Some f.Value
    let lineNo = let ln = mline.Groups.["lineNo"] in if String.IsNullOrWhiteSpace ln.Value then None else Some (int ln.Value)
    let sf = StackFrame(site, ?file=file, ?lineNo=lineNo)
    Match (StacktraceLine.Frame sf)
  elif mdelim.Success then
    Match StacktraceLine.Delim
  else
    let mexnt = mexnt.Match line
    if mexnt.Success then
      Match (StacktraceLine.ExnType (mexnt.Groups.["exnt"].Value, mexnt.Groups.["msg"].Value))
    elif not (String.IsNullOrWhiteSpace line) then
      Match (StacktraceLine.Text line)
    else
      NoMatch

let tryParse (st: string): ErrorInfo option =
  if String.IsNullOrWhiteSpace st then None else

  let res =
    st.Split([| "\r\n"; "\n" |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> line.Trim())
    |> Array.choose (function
      | Match item ->
        Some item
      | NoMatch ->
        None)

  if Array.isEmpty res then None else

  let mutable outmost = None

  for line in res do
    match line with
    | StacktraceLine.ExnType (et, m) ->
      match outmost with
      | None ->
        outmost <- Some (ErrorInfo())
      | Some outer ->
        let newE = ErrorInfo(m, et)
        outer.inner <- Some newE
        outmost <- Some newE

    | StacktraceLine.Frame sf ->
      let e =
        match outmost with
        | None ->
          let e = ErrorInfo()
          outmost <- Some e
          e
        | Some e ->
          e
      e.stackTrace.frames.Add sf

    | StacktraceLine.Delim | StacktraceLine.Text _ -> ()

  outmost

