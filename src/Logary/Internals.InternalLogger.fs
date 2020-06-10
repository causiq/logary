namespace Logary.Internals

open Hopac
open Hopac.Infixes
open Hopac.Extensions
open NodaTime
open Logary
open Logary.Model
open Logary.Internals

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal InternalLogger =
  /// This logger is special: in the above case the Registry takes the responsibility of shutting down all targets, but
  /// this is a stand-alone logger that is used to log everything in Logary with, so it needs to capable of handling its
  /// own disposal. It must not throw under any circumstances.
  type T =
    private {
      addCh: Ch<TargetConf>
      shutdownCh: Ch<unit>
      messageCh: Ch<Model.LogaryMessageBase * Promise<unit> * Ch<ProcessResult>>
    }
  with
    member x.name = PointName [| "Logary" |]

    interface Logger with // internal logger
      member x.logWithAck (_, message) =
        let m = message.getAsBase EventMessage
        x.messageCh *<+->- fun replCh nack -> m, nack, replCh

      member x.name = x.name

      /// internal logger will pass all log msg to targets,
      /// so the min level on logger is Verbose
      /// let the internal logger targets decide which will be accepted
      /// so this property is generally useless
      member x.level = LogLevel.Verbose

  let create ri =
    let addCh, messageCh, shutdownCh = Ch (), Ch (), Ch ()
    let api = { addCh = addCh; messageCh = messageCh; shutdownCh = shutdownCh}

    let rec iserver targets =
      Alt.choose [
        addCh ^=> fun targetConf ->
          Target.create ri targetConf >>= fun t ->
          iserver [| yield! targets; yield t |]

        messageCh ^=> fun (message, nack, replCh) ->
          let forwardToTarget = Target.logAllReduce Duration.Zero targets message ^=> Ch.give replCh
          (forwardToTarget <|> nack) ^=> fun () ->
          iserver targets

        shutdownCh ^=> fun () ->
          targets |> Seq.Con.iterJob (fun t -> Target.shutdown t ^=> id)
      ]

    Job.supervise ri.logger Policy.exponentialBackoffForever (iserver Array.empty)
    |> Job.startIgnore
    >>-. api

  let add (conf: TargetConf) (x: T): Job<unit> =
    Ch.give x.addCh conf
    :> Job<_>

  let shutdown (x: Logger) =
    match x with
    | :? T as ilogger -> Ch.give ilogger.shutdownCh ()
    | _ -> Alt.always ()