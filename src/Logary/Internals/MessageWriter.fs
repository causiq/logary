namespace Logary.Internals

open System.IO
open System.Threading
open System.Threading.Tasks
open Logary

type MessageWriter =
  abstract write: textWriter: TextWriter * message: LogaryMessage * cancellationToken: CancellationToken -> Task
