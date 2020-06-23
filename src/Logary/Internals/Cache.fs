module Logary.Internals.Cache

open System.Collections.Concurrent

/// Memoize the result based on the input parameter. This cache never expires
/// items.
let memoize<'input, 'output> (f: 'input -> 'output): 'input -> 'output =
  let cache = ConcurrentDictionary<'input, 'output>()
  fun x -> cache.GetOrAdd(x, f)

/// Copied from Logary.Internals.Cache, because disposing this target should
/// also remove the references to the loggers, whilst Logary.Internals.Cache
/// would have a static/global cache (more useful for type resolution that
/// won't change across instances).
let memoizeFactory<'input, 'output> (cache: ConcurrentDictionary<'input, 'output>) =
  fun (f: 'input -> 'output) ->
    fun x -> cache.GetOrAdd(x, f)
