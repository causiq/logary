namespace Logary.Internals

open Hopac
open Logary

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal ProcessResult =
  let success: ProcessResult = Ok Promise.unit

  let reduce (processResults: #seq<ProcessResult>): ProcessResult =
    let bundle (errors: Logary.ControlMessage list) =
      Model.ControlMessage(ControlMessageKind.Aggregated, Array.ofList errors)
        :> Logary.ControlMessage

    (([], []), processResults)
    ||> Seq.fold (fun (oks, errors) result ->
        match result with
        | Ok promise -> promise :: oks, errors
        | Result.Error error -> oks, error :: errors
     )
    |> function
      | [], [] -> success
      | promises, [] -> promises |> Job.conIgnore |> memo |> Ok
      | _, errors -> Result.Error (bundle errors)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal LogError =
  let targetBufferFull (targetName: string): ProcessResult =
    ControlMessageKind.BufferFull targetName
      |> Model.ControlMessage
      :> Logary.ControlMessage
      |> Result.Error

  let clientAbortLogging (): ProcessResult =
    ControlMessageKind.ClientAborted
      |> Model.ControlMessage
      :> Logary.ControlMessage
      |> Result.Error

  let registryClosed (): ProcessResult =
    ControlMessageKind.RegistryClosed
      |> Model.ControlMessage
      :> Logary.ControlMessage
      |> Result.Error


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal LogResult =
  let success: LogResult =
    ProcessResult.success
      |> Alt.always

  let error (message: Logary.ControlMessage): LogResult =
    Result.Error message
      |> Alt.always

  let targetBufferFull (targetName: string) =
    LogError.targetBufferFull targetName
      |> Alt.always

  let registryClosed () =
    LogError.registryClosed ()
      |> Alt.always