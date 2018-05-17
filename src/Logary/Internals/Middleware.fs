namespace Logary.Internals

open Logary

/// The type-signature for middleware; next:(Message -> Message) -> message:Message -> Message.
type Middleware =
  (Message -> Message) -> Message -> Message