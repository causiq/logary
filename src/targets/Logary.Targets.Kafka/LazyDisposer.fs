namespace Logary.Targets

open System

[<Struct>]
type LazyDisposer<'a when 'a :> IDisposable>(dL: Lazy<'a>) =
  interface IDisposable with
    member x.Dispose() = if dL.IsValueCreated then dL.Value.Dispose()

