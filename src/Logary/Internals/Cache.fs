namespace Logary.Internals

module Cache =
  open System.Collections.Concurrent

  /// Memoize the result based on the input parameter. This cache never expires
  /// items.
  let memoize<'input, 'output> (f: 'input -> 'output): 'input -> 'output =
    let cache = ConcurrentDictionary<'input, 'output>()
    fun x -> cache.GetOrAdd(x, f)