[<AutoOpen>]
module Logary.Tests.Target

open System
open System.IO
open Hopac
open Logary.Internals.Chiron.Formatting
open Logary.Model
open NodaTime
open Expecto
open Expecto.Logging
open Expecto.Logging.Message

let internal logger = Log.create "Logary.Tests.Utils"

let env defaultValue k =
  match Environment.GetEnvironmentVariable k with
  | null when isNull defaultValue ->
    failwithf "Couldn't load key %s" k
  | null ->
    defaultValue
  | v ->
    v

open Logary
open Logary.Internals
open Logary.Targets
open Logary.Configuration

let clearStream (s: StringWriter) =
  let sb = s.GetStringBuilder ()
  let str = string sb
  sb.Clear () |> ignore
  str

let buildTextWriterTarget name =
  let out = new StringWriter()
  let writerConf = TextWriter.TextWriterConf.create(out, JSONMessageWriter(JsonFormattingOptions.Pretty))
  let twTargetConf = TextWriter.create writerConf name
  out, twTargetConf

let buildLogManagerWith configFac = job {
  let svc = Resource.create("svc", "localhost")
  let tname = "4test"
  let out, twTargetConf = buildTextWriterTarget tname
  // let iloggerConf = ILogger.Targets [ twTargetConf ]

  let! logm =
    Config.create svc
    |> Config.target twTargetConf
    |> Config.processing (Events.events |> Events.setTargets [tname])
    |> Config.disableGlobals
    |> configFac
    |> Config.build
  return logm, out
}

let buildLogManager () = buildLogManagerWith id

let emptyRuntime =
  memo (
    Config.createInternalTargets (ILogger.Console Verbose)
    |> Config.createInternalLogger (
      RuntimeInfo.create (Resource.create("logary-tests", "dev-machine"))
    )
    |> Job.map (fun (ri, _) -> ri)
  )

let nanos xs =
  Duration.FromTicks (xs / Constants.NanosPerTick)

module Internals =
  type TimeoutResult<'T> =
    | Success of 'T
    | TimedOut

let finaliseJob target =
  Alt.choose [
    Target.shutdown target
      |> Alt.afterJob id
      |> Alt.afterFun Internals.Success

    timeOutMillis 8000
      |> Alt.afterFun (fun _ -> Internals.TimedOut)
  ]

/// Finalise the target and assert it was finalised within 1000 milliseconds
let finalise target =
  finaliseJob target |> Alt.afterFun (function
    | Internals.TimedOut ->
      failtestf "Finalising target timed out: %A" target
    | Internals.Success _ ->
      ())

let logMsgWaitAndShutdown (targetApi: Target.T) (logCallBack: (_ -> Job<unit>) -> #Job<unit>) =
  let logAndWait message =
    job {
      do! logger.verboseWithBP (eventX (sprintf "Sending message to Target(%s)" targetApi.name))
      let! res = Target.log Duration.Zero targetApi message
      match res with
      | Ok ack ->
        do! logger.verboseWithBP (eventX (sprintf "Waiting for Target(%s) to ACK message" targetApi.name))
        do! ack
        do! logger.verboseWithBP (eventX (sprintf "Target(%s) ACKed message" targetApi.name))
      | Result.Error e ->
        failtestf "%A" e
    }
  let finaliseJob =
    job {
      do! logger.verboseWithBP (eventX "Finalising target")
      do! finalise targetApi
      do! logger.verboseWithBP (eventX "Target finalised!")
    }

  Job.tryFinallyJob (logCallBack logAndWait) finaliseJob

