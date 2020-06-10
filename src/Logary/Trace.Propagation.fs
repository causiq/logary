namespace Logary.Trace.Propagation

open Logary.Trace

/// Gets a (key: string, value: string[]) pair from some object-like data structure. Getters return a list of
/// headers; for the given key/prefix passed. It's up to each getter's implementation to decide whether to interpret
/// the passed string as a prefix (like in the case of Jaeger baggage) or as a complete string (all other cases)
type Getter<'t> =
  't
    -> (* key / prefix *) string
    -> (* all key-values pairs *) ( (* key *) string * (* values *) string list) list

/// Sets a (key: string, value: string[]) pair into some object-like data structure
type Setter<'t> = string * string list -> 't -> 't

/// A propagator is responsible for reading/writing into a carrier. A specific implementation of
/// a propagator may know the Jaeger `uber-trace-id` format, of the Zipkin B3 format or the W3C Trace Context format.
type Propagator =
  /// Extracts:
  /// - Span attributes (e.g. debug-id, correlation-id, request-id),
  /// - (distributed) Context attributes, and
  /// - the Span context from the carrier.
  abstract extract:
    getter: Getter<'t> * carrier: 't
      -> spanAttrs: SpanAttr list * spanContextO: SpanContext option
  /// Injects the given (distributed) context attributes and span context (parent id, trace-state) into the target carrier.
  abstract inject:
    setter: Setter<'t> * spanContext: SpanContext * target: 't
      -> 't

