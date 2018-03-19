namespace Logary.Formatting


type StacktraceLine =
  { site: string
    file: string option
    lineNo: int option }

module DotNetStacktrace =
  open System
  open System.Text.RegularExpressions

  let private sample = """  at Logary.Targets.InfluxDb.Impl.extractMessage(TargetMessage request) in /Users/h/dev/tradera/logary/src/targets/Logary.Targets.InfluxDb/Targets_InfluxDb.fs:line 278
  at Microsoft.FSharp.Collections.Internal.IEnumerator.map@74.DoMoveNext(b& curr)
  at Microsoft.FSharp.Collections.Internal.IEnumerator.MapEnumerator`1.System-Collections-IEnumerator-MoveNext()
  at System.String.Join(String separator, IEnumerable`1 values)
  at Logary.Targets.InfluxDb.Impl.x2yJ@1-2(InfluxDbConf conf, RuntimeInfo ri, TargetAPI api, Uri endpoint, HttpClient client, TargetMessage[] reqs) in /Users/h/dev/tradera/logary/src/targets/Logary.Targets.InfluxDb/Targets_InfluxDb.fs:line 308
  at Hopac.Core.ContBind`2.DoWork(Worker& wr)
  at Hopac.Core.Worker.Run(Scheduler sr, Int32 me)"""

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
