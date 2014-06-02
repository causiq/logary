namespace Logary

module Gauges =
  open Logary
  open Logary.Target

  open Rules
  open Targets
  open Registry

  open Swensen.Unquote
  open NUnit.Framework
  open TestDSL
  open TextWriter

  [<Test>]
  let ``sending timing`` () =
    let target =
      confTarget "stubMetricWriter" (create (TextWriterConf.Default(stdout, stderr)))
      |> validateTarget

    let targets = [ target ]

    //TODO
    ()
