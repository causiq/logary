namespace Logary

open System.Collections.Generic

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


/// https://github.com/census-instrumentation/opencensus-proto/blob/master/src/opencensus/proto/trace/v1/trace.proto#L346-L395
type StackTrace(?stackFrames: StackFrame[], ?droppedFramesCount) =
  let fr: ResizeArray<_> =
    stackFrames
      |> Option.map ResizeArray<_>
      |> Option.defaultWith (fun () -> ResizeArray<_>())
  member val frames: IList<StackFrame> = fr :> _ with get, set
  member val droppedFramesCount: uint16 = defaultArg droppedFramesCount 0us with get, set


type ErrorInfo(?message, ?errorType, ?stackTrace, ?inner) =
  let st = stackTrace |> Option.defaultWith StackTrace
  member val message: string option = message with get, set
  member val errorType: string option = errorType with get, set
  member val stackTrace: StackTrace = st with get, set
  member val inner: ErrorInfo option = inner with get, set

