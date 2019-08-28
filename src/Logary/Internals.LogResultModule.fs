namespace Logary

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal LogResult =
  open Hopac

  let success: LogResult = Alt.always (Result.Ok Promise.unit)
  let error message : LogResult = Alt.always (Result.Error message)

  let notAcceptedByTarget (targetName: string) = error (Message.eventFormat("not accept by {target}", targetName))

module internal LogError =
  let targetBufferFull (targetName: string): ProcessResult = Result.Error (Message.eventFormat("{target} 's log buffer is full", targetName))

  let timeOutToPutBuffer (targetName: string) (waitSeconds: double): ProcessResult = Result.Error (Message.eventFormat("time out: waiting {duration} seconds to putting message on to {target} 's log buffer", waitSeconds, targetName))

  let clientAbortLogging: ProcessResult = Result.Error (Message.eventFormat("client abort logging"))

  let registryClosed: ProcessResult = Result.Error (Message.eventFormat("registry has been shutdown"))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal ProcessResult =
  open Hopac

  let success: ProcessResult = Ok (Promise.unit)

  let reduce (processResults: #seq<ProcessResult>): ProcessResult =
    let errorMsg (errors: Message list) = Message.eventFormat ("some targets processing failed: {errors}", errors)
    (([], []), processResults)
    ||> Seq.fold (fun (oks, errors) result ->
        match result with
        | Ok promise -> promise :: oks, errors
        | Result.Error error -> oks, error :: errors
     )
    |> function
      | [], [] -> success
      | promises, [] -> promises |> Job.conIgnore |> memo |> Ok
      | _, errors -> Result.Error (errorMsg errors)
