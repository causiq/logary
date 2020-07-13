namespace Logary

open System
open System.Collections.Generic
open System.IO
open System.Text

type ModuleInfo(?name, ?buildId) =
  /// For example: main binary, kernel modules, and dynamic libraries such as libc.so, sharedlib.so, `index.tsx` or
  /// `bundle-3-df151ef.js`, or `src/components/Page.tsx`
  member val name: string option = name with get, set
  /// The build id (or git commit if using deterministic builds) this this module comes from.
  member val buildId: string option = buildId with get, set


type StackFrame(?site, ?file, ?functionName, ?originalFunctionName, ?lineNo, ?colNo, ?sourceVersion, ?loadModule) =
  member val site: string option = site with get, set
  member val file: string option = file with get, set
  member val functionName: string option = functionName with get, set
  member val originalFunctionName: string option = originalFunctionName with get, set
  member val lineNo: int option = lineNo with get, set
  member val colNo: int option = colNo with get, set
  member val sourceVersion: string option = sourceVersion with get, set
  member val loadModule: ModuleInfo option = loadModule with get, set

  member x.writeTo (writer: TextWriter) =
    [ x.site
      x.functionName
      if x.file.IsSome then
        Some " in "
        x.file
        if x.lineNo.IsSome then
          Some ":"
          Some (x.lineNo.Value.ToString())
        if x.colNo.IsSome then
          if x.lineNo.IsSome then Some ","
          Some (x.colNo.Value.ToString())
    ]
    |> Seq.choose id
    |> Seq.iter writer.Write

  override x.ToString() =
    let sb = StringBuilder()
    use sw = new StringWriter(sb)
    x.writeTo sw
    sb.ToString()


/// https://github.com/census-instrumentation/opencensus-proto/blob/master/src/opencensus/proto/trace/v1/trace.proto#L346-L395
type StackTrace(?stackFrames: StackFrame[], ?droppedFramesCount) =
  let fr: ResizeArray<_> =
    stackFrames
      |> Option.map ResizeArray<_>
      |> Option.defaultWith (fun () -> ResizeArray<_>())
  member val frames: IList<StackFrame> = fr :> _ with get, set
  member val droppedFramesCount: uint16 = defaultArg droppedFramesCount 0us with get, set

  member x.writeTo (writer: TextWriter) =
    let mutable first = true
    for frame in x.frames do
      if not first then
        writer.WriteLine()
      frame.writeTo writer
      first <- false

  override x.ToString() =
    let sb = StringBuilder()
    use sw = new StringWriter(sb)
    x.writeTo sw
    sb.ToString()

type ErrorInfo(?message, ?errorType, ?stackTrace, ?inner) =
  let st = stackTrace |> Option.defaultWith StackTrace
  member val message: string option = message with get, set
  member val errorType: string option = errorType with get, set
  member val stackTrace: StackTrace = st with get, set
  member val inner: ErrorInfo option = inner with get, set

  interface IEquatable<ErrorInfo> with
    member x.Equals o =
      x.message = o.message
      && x.errorType = o.errorType

  interface IValueFormattable with
    member x.toKeyValues baseKey =
      let d = Dictionary<string, Value>()
      let addStr (k: string) (s: string) = d.Add(sprintf "%s.%s" baseKey k, Value.Str s)
      x.message |> Option.iter (addStr "message")
      x.errorType |> Option.iter (addStr "errorType")
      x.stackTrace |> (sprintf "%O" >> addStr "stackTrace")
      x.inner |> Option.iter (fun x ->
        let vf = x :> IValueFormattable
        match vf.toKeyValues(sprintf "%s.inner" baseKey) with
        | Choice1Of2 kv -> d.Add(kv.Key, kv.Value)
        | Choice2Of2 kvs -> for kv in kvs do d.Add(kv.Key, kv.Value)
      )
      d :> IReadOnlyDictionary<string, Value>
        |> Choice2Of2
