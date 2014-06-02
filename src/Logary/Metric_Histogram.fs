namespace Logary

module Histograms =
  let x () = ()

  // https://github.com/codahale/metrics/blob/master/metrics-core/src/main/java/com/codahale/metrics/Reservoir.java

  type Reservoir =
    { size   : uint32
    ; update : uint64 -> Reservoir }

  // https://github.com/codahale/metrics/blob/master/metrics-core/src/main/java/com/codahale/metrics/ExponentiallyDecayingReservoir.java
  // http://dimacs.rutgers.edu/~graham/pubs/papers/fwddecay.pdf

  // https://github.com/codahale/metrics/blob/master/metrics-core/src/main/java/com/codahale/metrics/UniformReservoir.java
  // http://www.cs.umd.edu/~samir/498/vitter.pdf
  // http://lukerandall.github.io/2012/04/20/implementing-the-reservoir-algorithm-in-haskell.html
  // https://en.wikipedia.org/wiki/Reservoir_sampling
