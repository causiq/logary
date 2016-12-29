namespace Logary

/// A StringFormatter is the thing that takes a message and returns it as a
/// string that can be printed, sent or otherwise dealt with in a manner that
/// suits the target.
type MessageFormatter =
  abstract format : Message -> TextWriter -> unit