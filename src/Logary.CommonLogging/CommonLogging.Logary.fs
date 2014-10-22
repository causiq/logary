module Impl

open System

open Common.Logging

open Logary

type internal Adapter(l : Logger) =
  interface ILog with
    // TODO: implement me
    member x.IsTraceEnabled = l.Level >= Verbose
    // etc

type LogaryAdapter(lm : LogManager) =
  interface ILoggerFactoryAdapter with
    member x.GetLogger (typ : Type) =
      Adapter(lm.GetLogger(typ.FullName)) :> ILog
    member x.GetLogger (name : string) =
      lm.GetLogger name