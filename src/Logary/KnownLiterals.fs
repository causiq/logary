module Logary.KnownLiterals

[<Literal>]
let ErrorsFieldName = "errors"

/// The 
[<Literal>]
let ServiceContextName = "service"

/// The tags context field
[<Literal>]
let TagsContextName = "tags"

/// Used to tag gauges which are 'composite' at-the-same-instant measurements
/// of something. This makes targets aware that they should not send the main
/// PointValue
[<Literal>]
let SuppressPointValue = "suppress-point-value"