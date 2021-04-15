namespace Logary.Internals

#nowarn "9"

open System
open System.Buffers
open Logary.YoLo
open FSharp.NativeInterop

module private StringsModule =

  /// https://stackoverflow.com/questions/35703731/f-nativeptr-stackalloc-slower-then-c-sharp-stackalloc-decompiled-code-include
  /// https://github.com/fsharp/fslang-suggestions/issues/720
  let inline stackalloc<'a when 'a: unmanaged> size =
    let p = NativePtr.stackalloc<'a> size |> NativePtr.toVoidPtr
    Span<'a>(p, size)

open StringsModule

type Strings =

  /// https://stackoverflow.com/questions/56979469/parse-utf8-string-from-readonlysequencebyte
  static member parseAsUTF8 (slice: ReadOnlySequence<byte>, ?strLenEst) =
      if slice.IsSingleSegment then UTF8.encoding.GetString(slice.FirstSpan) else

      let strLenEst = defaultArg strLenEst (int slice.Length)

      let decoder = UTF8.encoding.GetDecoder()
      let mutable preProcessedBytes = 0
      let mutable processedCharacters = 0
      let characterSpan = stackalloc<char> strLenEst

      for memory in slice do
        preProcessedBytes <- preProcessedBytes + memory.Length
        let isLast = preProcessedBytes = int slice.Length
        let emptyCharSlice = characterSpan.Slice(processedCharacters, characterSpan.Length - processedCharacters)
        let charCount = decoder.GetChars(memory.Span, emptyCharSlice, isLast)
        processedCharacters <- processedCharacters + charCount

      let finalCharacters = characterSpan.Slice(0, processedCharacters)
      new string(Span.op_Implicit finalCharacters)