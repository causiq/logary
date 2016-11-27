module Logary.Targets.Mailgun

open Hopac
open Hopac.Infixes
open Logary
open Logary.Target
open Logary.Message
open Logary.Internals
open Mailgun.Api
open System.Net.Mail

type Domain = string

type MailgunLogaryConf =
  { mailgun    : Configured
    templater  : Logary.Message -> MailBody
    getOpts    : Domain * Logary.Message -> SendOpts
    msgFactory : MailgunLogaryConf -> MailBody * SendOpts * Logary.Message -> Message
    from       : MailAddress
    ``to``     : MailAddress list
    cc         : MailAddress list
    bcc        : MailAddress list
    /// Minimum inclusive level to send e-mail for
    minLevel   : LogLevel
    domain     : Domain }

module internal Impl =

  let templater msg =
    let formatter = Formatting.StringFormatter.levelDatetimeMessagePathNl
    TextBody (formatter.format msg)

  let getOpts (domain, msg) = SendOpts.Create domain

  let msgFactory conf (body, opts, msg) =
    { from        = conf.from
      ``to``      = conf.``to``
      cc          = conf.cc
      bcc         = conf.bcc
      inlineImgs  = []
      // CONSIDER: If the message is a measure, the subject line will be just the value with unit.
      subject     = Formatting.MessageParts.formatValueShallow msg
      body        = body
      attachments = [] }

  let loop (conf : MailgunLogaryConf)
           (ri : RuntimeInfo)
           (requests : RingBuffer<TargetMessage>)
           (shutdown : Ch<IVar<unit>>)
           : Job<unit>=

    let rec loop () : Job<unit> =
      Alt.choose [
        shutdown ^=> fun ack ->
          ack *<= ()

        RingBuffer.take requests ^=> function
          | Log (logMsg, ack) ->
            if logMsg.level < conf.minLevel then loop () else
            let body  = conf.templater logMsg
            let opts  = conf.getOpts (conf.domain, logMsg)
            let msg   = conf.msgFactory conf (body, opts, logMsg)

            job {
              let! resp = Messages.send conf.mailgun opts msg
              match resp with
              | Result response ->
                use x = response
                ()

              | x ->
                do! ri.logger.errorWithBP (
                      eventX "Unknown response from Mailgun received."
                      >> setFieldFromObject "response" x)

              do! ack *<= ()
              return! loop ()
            }

          | Flush (ack, nack) ->
            Alt.choose [
              Ch.give ack ()
              nack :> Alt<_>
            ] ^=> loop
            :> Job<_>

      ] :> Job<_>

    if conf.``to`` = [] then
      upcast (ri.logger.errorWithBP (eventX "No `to` was configured in Mailgun target"))
    elif conf.from.Host = "example.com" then
      upcast (ri.logger.errorWithBP (eventX "You cannot send e-mail to example.com in Mailgun target"))
    elif conf.domain = "example.com" then
      upcast (ri.logger.errorWithBP (eventX "You cannot send e-mail from example.com in Mailgun target"))
    else
      loop ()

let empty =
  { mailgun    = { apiKey = "" }
    templater  = Impl.templater
    getOpts    = Impl.getOpts
    msgFactory = Impl.msgFactory
    from       = MailAddress "fixme@example.com"
    ``to``     = []
    cc         = []
    bcc        = []
    minLevel   = LogLevel.Error
    domain     = "example.com"
  }

type MailgunLogaryConf with
  static member Create(from      : MailAddress,
                       ``to``    : MailAddress list,
                       mailgun   : Configured,
                       domain    : Domain,
                       ?minLevel : LogLevel,
                       ?cc : MailAddress list,
                       ?bcc : MailAddress list,
                       ?getOpts : _) =
    { empty with
        mailgun  = mailgun
        domain   = domain
        from     = from
        ``to``   = ``to``
        minLevel = defaultArg minLevel empty.minLevel
        cc       = defaultArg cc empty.cc
        bcc      = defaultArg bcc empty.bcc
        getOpts  = defaultArg getOpts empty.getOpts }

/// Create a new Mailgun target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// Use with LogaryFactory.New( s => s.Target<Mailgun.Builder>() )
type Builder(conf : MailgunLogaryConf, callParent : FactoryApi.ParentCallback<Builder>) =
  member x.MailData(from : MailAddress, ``to`` : MailAddress list, domain : Domain) =
    Builder({ conf with from   = from
                        ``to`` = ``to``
                        domain = domain }, callParent)

  member x.Mailgun(config : Configured) =
    ! (callParent <| Builder({ conf with mailgun = config }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
