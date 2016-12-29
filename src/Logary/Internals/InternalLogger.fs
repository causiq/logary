namespace Logary.Internals

open System
open Hopac
open Hopac.Infixes
open Logary
open Logary.Internals

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InternalLogger =
  /// This logger is special: in the above case the Registry takes the responsibility
  /// of shutting down all targets, but this is a stand-alone logger that is used
  /// to log everything in Logary with, so it needs to capable of handling its
  /// own disposal. It must not throw under any circumstances.
  type T =
    private {
      rule : Rule
      addCh : Ch<TargetConf>
      shutdownCh : Ch<unit>
      messageCh : Ch<Message * Promise<unit> * Ch<Promise<unit>>>
    }

    interface Logger with
      member x.logWithAck logLevel messageFactory =
        if logLevel >= x.rule.level then
          let message = messageFactory logLevel
          x.messageCh *<+->- fun replCh nack -> message, nack, replCh
        else
          Promise.instaPromise 

      member x.log logLevel messageFactory =
        if logLevel >= x.rule.level then
          let me : Logger = upcast x
          me.logWithAck logLevel messageFactory // delegate down
          |> Alt.afterFun (fun _ -> ())
        else
          Alt.always ()

      member x.level =
        x.rule.level

      member x.name =
        PointName [| "Logary" |]

  let create ri rule ts =
    let addCh, messageCh, shutdownCh = Ch (), Ch (), Ch ()

    let rec server targets =
      Alt.choose [
        addCh ^=> fun targetConf ->
          Target.create ri targetConf >>= fun t ->
          server (t :: targets)

        messageCh ^=> fun (message, nack, replCh) ->
          Alt.choose [
            Target.logAll targets message ^=> fun ack ->
              replCh *<- ack ^=> fun () ->
              server targets

            nack ^=> fun () -> server targets
          ]

        shutdownCh ^=> fun () ->
          targets
          |> Seq.map Target.shutdown
          |> Job.conCollect // await buffer
          |> Job.bind Job.conCollect // await ack
          |> Job.Ignore
      ]

    server [] >>-.
    { addCh = addCh
      messageCh = messageCh
      shutdownCh = shutdownCh
      rule = rule }

  let add (conf : TargetConf) (x : T) : Job<unit> =
    Ch.give x.addCh conf
    :> Job<_>

  let shutdown (x : T) : Alt<unit> =
    Ch.give x.shutdownCh ()