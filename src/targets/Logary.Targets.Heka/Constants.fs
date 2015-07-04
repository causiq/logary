module Logary.Heka.Constants

let HeaderDelimiterSize = 2u                       // record separator + len
let HeaderFramingSize   = HeaderDelimiterSize + 1u // unit separator
let MaxHeaderSize       = 255u
let RecordSeparator     = byte 0x1e
let UnitSeparator       = byte 0x1f
let UuidSize            = 16u
