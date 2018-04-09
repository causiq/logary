namespace Logary.Formatting

type StacktraceLineData =
  { site: string
    file: string option
    lineNo: int option }

  static member create site file lineNo =
    { site = site; file = file; lineNo = lineNo }

type StacktraceLine =
  /// "    at ServiceStack.ServiceClient.Web.ServiceClientBase.Send[TResponse](String httpMethod, String relativeOrAbsoluteUrl, Object request)"
  | Line of line:StacktraceLineData
  /// "--- End of inner exception stack trace ---"
  | InnerDelim

module DotNetStacktrace =
  open System
  open System.Text.RegularExpressions

  let (|Match|NoMatch|) (line: string) =
    let mline = Regex.Match(line, @"at (?<site>.+?)(( in )((?<file>[^\n]+?)(:line (?<lineNo>\d{1,}))?))?$", RegexOptions.Multiline ||| RegexOptions.IgnoreCase)
    let mdelim = Regex.Match(line, @"\S*--- [a-zA-Z ]+---", RegexOptions.Multiline ||| RegexOptions.IgnoreCase)
    if mline.Success then
      let site = mline.Groups.["site"].Value
      let file = let f = mline.Groups.["file"] in if not f.Success || String.IsNullOrWhiteSpace f.Value then None else Some f.Value
      let lineNo = let ln = mline.Groups.["lineNo"] in if String.IsNullOrWhiteSpace ln.Value then None else Some (int ln.Value)
      Match (Line { site = site; file = file; lineNo = lineNo })
    elif mdelim.Success then
      Match InnerDelim
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