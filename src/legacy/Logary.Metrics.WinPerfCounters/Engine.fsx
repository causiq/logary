#I "bin/Release/net461"
#r "NodaTime.dll"
#r "Hopac.Core.dll"
#r "Hopac.dll"
open NodaTime
open Hopac
open Hopac.Infixes
#r "NodaTime.dll"
#r "Logary.dll"
open Logary
open System

let inline nilj<'x> = Job.result Stream.Nil :> Job<Stream.Cons<'x>>
let inline nil<'x> = Promise Stream.Nil :> Stream<'x>
let inline error (e: exn) = Promise<Stream.Cons<_>> e :> Stream<_>

let inline consj x xs = Job.result (Stream.Cons (x, xs))
let inline consa x xs = Alt.always (Stream.Cons (x, xs))

let rec window' (b: ResizeArray<_>) (timeOut : Job<_>) (xs : Stream<_>) clock : Job<Stream.Cons<ResizeArray<_>>> =
      Alt.tryIn xs (function
        | Stream.Cons (x, xs) ->
          printfn "Adding %A, len=%i" x b.Count
          b.Add x
          window' b timeOut xs clock
        | Stream.Nil ->
          consj b nil)
        (fun e -> consj b (Stream.error e))

  <|> clock ^=> fun _ ->
        consj b (window timeOut xs)

   :> Job<_>

and window (timeOut : Job<_>) (xs : Stream<'x>) : Stream<ResizeArray<'x>> =
  Job.tryIn xs (function
    | Stream.Cons (x, xs) ->
      let b = ResizeArray<_> (1)
      b.Add x
      window' b timeOut xs (memo timeOut)
    | Stream.Nil ->
      nilj)
    Stream.error
  |> memo

let src : Stream<int> =
  Stream.unfoldFun (fun (rnd : Random) -> Some (rnd.Next(10), rnd)) (Random())
  |> Stream.afterEach (timeOutMillis 200)

let ms600 = timeOutMillis 2000
window ms600 src
|> Stream.mapFun (Seq.toList >> List.groupBy id >> List.ofSeq)
|> Stream.consumeFun (printfn "%A")

