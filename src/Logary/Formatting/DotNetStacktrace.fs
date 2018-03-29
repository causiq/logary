namespace Logary.Formatting

type StacktraceLine =
  { site: string
    file: string option
    lineNo: int option }
  static member create site file lineNo =
    { site = site; file = file; lineNo = lineNo }

module DotNetStacktrace =
  open System
  open System.Text.RegularExpressions
  let parse (st: string): StacktraceLine[] =
    if String.IsNullOrWhiteSpace st then Array.empty else
    st.Split([| "\r\n"; "\n" |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> line.Trim())
    |> Array.map (fun line ->
        let m = Regex.Match(line, @"at (?<site>.+?)(( in )((?<file>[^\n:]+)(:line (?<lineNo>\d{1,}))?))?$", RegexOptions.Multiline ||| RegexOptions.IgnoreCase)
        let site = m.Groups.["site"].Value
        let file = let f = m.Groups.["file"] in if not f.Success || String.IsNullOrWhiteSpace f.Value then None else Some f.Value
        let lineNo = let ln = m.Groups.["lineNo"] in if String.IsNullOrWhiteSpace ln.Value then None else Some (int ln.Value)
        { site = site
          file = file
          lineNo = lineNo })
