module internal Types

// see https://github.com/twitter/zipkin/blob/master/zipkin-thrift/src/main/resources/thrift/zipkinCore.thrift
// these are the annotations we always expect to find in a span
let [<Literal>] CLIENT_SEND = "cs"
let [<Literal>] CLIENT_RECV = "cr"
let [<Literal>] SERVER_SEND = "ss"
let [<Literal>] SERVER_RECV = "sr"

type Endpoint =
  { ipv4         : uint32
  ; port         : uint16
  ; service_name : string } // which service did this operation happen on?

type AnnotationType =
  | BOOL
  | BYTES
  | I16
  | I32
  | I64
  | DOUBLE
  | STRING

type BinaryAnnotation =
  { key             : string
  ; value           : byte array
  ; annotation_type : AnnotationType
  ; host            : Endpoint option }

type ZipkinAnnotation =
  { timestamp       : uint64           // microseconds since epoch
  ; value           : string           // what happened at the timestamp?
  ; host            : Endpoint option  // host this happened on
  ; duration        : uint32 option }  // how long did the operation take? microseconds

type TraceId = uint64
type SpanId = uint64

/// identifies an individual request
type Span =
  { trace_id           : TraceId                    // unique trace id, use for all spans in trace
  ; name               : string option              // span name, rpc method for example
  ; id                 : SpanId                     // unique span id, only used for this span
  ; parent_id          : SpanId option              // optional parent span id
  ; annotations        : ZipkinAnnotation list      // list of all annotations/events that occured
  ; binary_annotations : BinaryAnnotation list      // list of all binary annotations
  ; debug              : bool                       // if true, we DEMAND that this span passes all samplers
  }

/// Another important part of the tracing is the light weight header we use to pass information between the traced services. The tracing header consists of the following:
type TraceHeader =
  /// identifies the whole trace
  { trace_id       : TraceId
  /// identifies an individual request
  ; span_id        : SpanId
  /// Added if this request was made as part of another request
  ; parent_span_id : SpanId option
  /// tells us if we should log the tracing data or not
  ; sampled        : bool }
