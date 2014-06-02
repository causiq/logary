namespace Logary

module Fac =
  open System.IO
  open System.Text
  open FSharp.Actor
  
  open Logary
  open Logary.Target.TextWriter

  open Logary.Configuration.Config
  open Rules
  open System.Text.RegularExpressions
  open Targets

  let emptyTarget =
    { name  = "empty target"
    ; actor = Actor.spawn Actor.Options.Default (fun _ -> async { return () }) }

  let emptyRule =
    { hiera  = Regex(".*")
    ; target = "empty target"
    ; accept = fun line -> true
    ; level  = Verbose }

  let textWriter () =
    let sb = new StringBuilder()
    new StringWriter(sb)

  let withLogary f =
    let out, err = textWriter (), textWriter ()

    let target = confTarget "cons" (create <| TextWriterConf.Default(out, err))

    let rule =
      { accept = fun ll -> true
      ; hiera = Regex(".*")
      ; level = LogLevel.Verbose
      ; target = target.name }

    let logary =
      confLogary "tests"
      |> withRules   [ rule ]
      |> withTargets [ target ]
      |> validateLogary
      |> runLogary

    f logary out err
