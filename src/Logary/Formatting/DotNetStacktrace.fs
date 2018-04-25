namespace Logary.Formatting

type StacktraceLineData =
  { site: string
    file: string option
    lineNo: int option }

  static member create site file lineNo =
    { site = site; file = file; lineNo = lineNo }

type StacktraceLine =
  /// WRN: Assembly binding logging is turned OFF.
  | LineOutput of data:string
  /// System.IO.FileNotFoundException: Could not load file or assembly 'Google.Api.Gax.Rest, Version=2.2.1.0, Culture=neutral, PublicKeyToken=3ec5ea7f18953e47' or one of its dependencies. The system cannot find the file specified.
  | ExnType of exnType:string * message:string
  /// "    at ServiceStack.ServiceClient.Web.ServiceClientBase.Send[TResponse](String httpMethod, String relativeOrAbsoluteUrl, Object request)"
  | Line of line:StacktraceLineData
  /// "--- End of inner exception stack trace ---"
  | InnerDelim

module DotNetStacktrace =
  open System
  open System.Text.RegularExpressions

  let internal mline = Regex(@"at (?<site>.+?)(( in )((?<file>[^\n]+?)(:line (?<lineNo>\d{1,}))?))?$", RegexOptions.Multiline ||| RegexOptions.IgnoreCase)
  let internal mdelim = Regex(@"\S*--- [a-zA-Z ]+---", RegexOptions.Multiline ||| RegexOptions.IgnoreCase)
  let internal mexnt = Regex(@"^(?<exnt>[a-z0-9\.+`]+Exception([0-9`+]+)?): (?<msg>.+)$", RegexOptions.Multiline ||| RegexOptions.IgnoreCase)

  let (|Match|NoMatch|) (line: string) =
    let mline = mline.Match line
    let mdelim = mdelim.Match line
    if mline.Success then
      let site = mline.Groups.["site"].Value
      let file = let f = mline.Groups.["file"] in if not f.Success || String.IsNullOrWhiteSpace f.Value then None else Some f.Value
      let lineNo = let ln = mline.Groups.["lineNo"] in if String.IsNullOrWhiteSpace ln.Value then None else Some (int ln.Value)
      Match (Line { site = site; file = file; lineNo = lineNo })
    elif mdelim.Success then
      Match InnerDelim
    else
      let mexnt = mexnt.Match line
      if mexnt.Success then
        Match (ExnType (mexnt.Groups.["exnt"].Value, mexnt.Groups.["msg"].Value))
      elif not (String.IsNullOrWhiteSpace line) then
        Match (LineOutput line)
      else
        NoMatch

  let parse (st: string) =
    if String.IsNullOrWhiteSpace st then Array.empty else
    st.Split([| "\r\n"; "\n" |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> line.Trim())
    |> Array.choose (function
      | Match item ->
        Some item
      | NoMatch ->
        None)