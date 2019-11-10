module internal Logary.Internals.Regex

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern, RegexOptions.IgnorePatternWhitespace ||| RegexOptions.IgnoreCase)
  if m.Success then
    seq { for g in m.Groups -> g }
      |> Seq.filter (fun g -> g.Success)
      |> Seq.map (fun g -> g.Value)
      |> List.ofSeq
      |> List.tail
      |> Some
  else None