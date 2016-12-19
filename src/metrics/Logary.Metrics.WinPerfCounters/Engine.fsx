#I "bin/Release"
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

#r "../../../packages/Microsoft.Bcl.Async/lib/net40/Microsoft.Threading.Tasks.Extensions.Desktop.dll"
#r "../../../packages/Microsoft.Bcl.Async/lib/net40/Microsoft.Threading.Tasks.Extensions.dll"
#r "../../../packages/Microsoft.Bcl.Async/lib/net40/Microsoft.Threading.Tasks.dll"
#r "../../../packages/Microsoft.Bcl/lib/net40/System.IO.dll"
#r "../../../packages/Microsoft.Net.Http/lib/net40/System.Net.Http.dll"
#r "../../../packages/Microsoft.Net.Http/lib/net40/System.Net.Http.Extensions.dll"
#r "../../../packages/Microsoft.Net.Http/lib/net45/System.Net.Http.Primitives.dll"
#r "../../../packages/Autofac/lib/net45/Autofac.dll"
#r "../../../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
#r "../../../packages/TweetinviAPI/lib/net45/Tweetinvi.dll"

open Tweetinvi
open Tweetinvi.Streaming
open Tweetinvi.Models
open Tweetinvi.Core.Extensions
open Tweetinvi.Models.DTO
open Tweetinvi.Json
open System.Text.RegularExpressions

Auth.SetUserCredentials("1DHl3z80zOMAjnR4O2hgSkFHy", "fszEb2aHf5cIwTBYvNivZbJByucsjApBwv2Po047WhYqBsONTJ", "213763729-lQx57HsQxrIGsnqEa3yaOK0HUADaEjElETlnJncv", "j1LnS07jGQmMsiJGF0tOVF3x6Ueziro7ss7zYYZCSD5pk")
TweetinviEvents.QueryBeforeExecute.Subscribe(fun args -> printfn "Query url=%s" args.QueryURL)

//let u = User.GetAuthenticatedUser()
//printfn "user=%O" u

let tweets =
  let stream = Stream.CreateSampleStream()
  let mb = BoundedMb 50
  stream.TweetReceived.Add (BoundedMb.put mb >> run)
  let stopped = IVar ()
  stream.StreamStopped.Add (IVar.fill stopped >> run)
  stream.AddTweetLanguageFilter(LanguageFilter.English)
  //printfn "Starting Twitter stream"
  stream.StartStreamAsync() |> ignore
  //printfn "Creating Hopac stream"
  Stream.indefinitely (BoundedMb.take mb) |> Stream.takeUntil stopped

let hashtags (s : string) =
  let ms =
    Regex.Matches(s, @"#(\w+)", RegexOptions.IgnoreCase)
  if ms.Count > 0 then
    ms
    |> Seq.cast<Match>
    |> Seq.map (fun mx -> mx.Value.ToLowerInvariant())
    |> List.ofSeq
    |> Some
  else
    None

let m = hashtags "Hello world #how are #you today?"
//let m = hashtags "Hello world"


let stoppit : IVar<unit> = IVar ()
//IVar.fill stoppit () |> run

let tags =
  tweets
  |> Stream.chooseFun (fun args -> 
    hashtags args.Tweet.Text
    |> Option.map (fun tags -> args.Tweet, tags))

let events =
  let flip f a b = f b a
  tags
  |> Stream.mapFun (fun (tweet, tags) ->
    Message.event LogLevel.Info tweet.FullText
    |> (fun msg -> tags |> List.fold (flip Message.tag) msg)
    |> Message.setTimestamp (Instant.FromDateTimeUtc tweet.CreatedAt)
  )
  //|> Stream.mergeMap Stream.ofSeq

//tags |> Stream.consumeFun (fun tags -> printfn "%A" tags)

module Folds =

  let sum s (t : int64) = s + t
  let inc s t = s + 1L
  let dec s t = s - 1L

let trending =
  let add = tags |> Stream.mapFun (snd >> List.length) |> Stream.mapFun int64
  let sub = tags |> Stream.mapFun (snd >> List.length >> ((*) -1)) |> Stream.mapFun int64 |> Stream.shift (timeOutMillis (5000))

  add
  |> Stream.merge sub
  |> Stream.scanFun Folds.sum 0L

trending
|> Stream.consumeFun (fun count -> printfn "Current count=%i" count)

(*
; http://spootnik.org/entries/2014/01/14_real-time-twitter-trending-on-a-budget-with-riemann.html
(let [store    (index)
      trending (top 10 (juxt :metric :time) (tag "top" store) store)]
  (streams
    (by :service (moving-time-window 3600 (smap folds/sum trending)))))
*)

