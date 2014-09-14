module Logary.Targets.AWSCloudWatch

// The MIT License (MIT)
// 
// Copyright (c) 2014 Yan Cui https://github.com/theburningmonk/Metricano/blob/master/src/Metricano.CloudWatch/CloudWatchPublisher.fs
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// with minor changes by Henrik Feldt 2014

open FSharp.Actor

open Logary
open Logary.Target
open Logary.Internals

open System
open System.Collections.Generic
open System.Linq
open System.Threading
open System.Threading.Tasks

open Amazon.CloudWatch
open Amazon.CloudWatch.Model

type AWSCreds =
  { accessKeyId     : string
    secretAccessKey : string
    region          : string }

type AWSCloudWatchConf =
  { rootNamespace : string
    awsCreds      : AWSCreds }

let empty =
  { rootNamespace = "ROOTNS"
    awsCreds      =
      { accessKeyId     = "MISSING KEY ID"
        secretAccessKey = "MISSING SECRET ACCESS KEY"
        region          = "MISSING REGION" } }

/// Exception that's thrown when attempting to merge two MetricDatum that cannot be merged
exception MetricDatumMismatch of MetricDatum * MetricDatum

module internal Impl =

  [<AutoOpen>]
  module Extensions = 
    /// Default function for calcuating delay (in milliseconds) between retries, based on (http://en.wikipedia.org/wiki/Exponential_backoff)
    /// After 8 retries the delay starts to become unreasonable for most scenarios, so cap the delay at that
    let private exponentialDelay =
      let calcDelay retries =
        let rec sum acc =
          function
          | 0 -> acc
          | n -> sum (acc + n) (n - 1)
      
        let n = pown 2 retries - 1
        let slots = float (sum 0 n) / float (n + 1)
        int (100.0 * slots)
    
      let delays = [| 0..8 |] |> Array.map calcDelay
      (fun retries -> delays.[min retries 8])
  
    type Async with
      /// Retries the async computation up to specified number of times. Optionally accepts a function to calculate
      /// the delay in milliseconds between retries, default is exponential delay with a backoff slot of 500ms.
      static member WithRetry(computation : Async<'a>, maxRetries, ?calcDelay) = 
        let calcDelay = defaultArg calcDelay exponentialDelay
      
        let rec loop retryCount = 
          async { 
            let! res = computation |> Async.Catch
            match res with
            | Choice2Of2 _ when retryCount < maxRetries ->
              do! calcDelay retryCount |> Async.Sleep
              return! loop (retryCount + 1)
            | _ -> return res
          }
        loop 0
  

  [<AutoOpen>]
  module Seq =
    /// Groups a sequence into gropus of at most the specified size
    /// Originally from http://fssnip.net/1o
    let inline groupsOfAtMost (size : int) (s : seq<'v>) : seq<'v []> = 
      seq { 
        let en = s.GetEnumerator()
        let more = ref true
        while !more do
          let group =
            [| let i = ref 0
               while !i < size && en.MoveNext() do
                 yield en.Current
                 i := !i + 1 |]
          if group.Length = 0 then more := false
          else yield group
      }

  [<RequireQualifiedAccess>]
  module Constants =
    // cloud watch limits the MetricDatum list to a size of 20 per request
    let putMetricDataListSize = 20

  type Measure with
    member x.ToMetricDatum() =
      let dim, unit = 
        match this with
        | Count _ -> new Dimension(Name = "Type", Value = "Count"), StandardUnit.Count
        | TimeSpan _ -> new Dimension(Name = "Type", Value = "TimeSpan"), StandardUnit.Milliseconds
      new MetricDatum(MetricName = this.Name, Unit = unit, Timestamp = x.TimeStamp,
                      Dimensions = new List<Dimension>([| dim |]),
                      StatisticValues = new StatisticSet(Maximum = x.Max, Minimum = x.Min, Sum = x.Sum, 
                                                         SampleCount = float x.SampleCount))

  let onPutMetricError = new Event<Exception>()
  
  let combine (left : MetricDatum) (right : MetricDatum) =
    if left.MetricName = right.MetricName && left.Unit = right.Unit then 
      left.StatisticValues.SampleCount <- left.StatisticValues.SampleCount + right.StatisticValues.SampleCount
      left.StatisticValues.Sum <- left.StatisticValues.Sum + right.StatisticValues.Sum
      left.StatisticValues.Maximum <- max left.StatisticValues.Maximum right.StatisticValues.Maximum
      left.StatisticValues.Minimum <- min left.StatisticValues.Minimum right.StatisticValues.Minimum
    else raise <| MetricDatumMismatch(left, right)
  
  let send ns (client : IAmazonCloudWatch) (datum : MetricDatum []) =
    async { 
      let req = new PutMetricDataRequest(Namespace = ns, MetricData = datum.ToList())
      let sendAsync = Async.FromBeginEnd(req, client.BeginPutMetricData, client.EndPutMetricData)
      let! res = Async.WithRetry(sendAsync, 3)
      match res with
      | Choice1Of2 _ -> ()
      | Choice2Of2 exn -> onPutMetricError.Trigger exn
    }
  
  let sendAll ns client (groups : MetricDatum [] seq) = 
    groups
    |> Seq.map (send ns client)
    |> Async.Parallel
    |> Async.Ignore

  type State =
    { metricData : Map<Named, MetricDatum>
      client     : IAmazonCloudWatch }

  let loop (conf : AWSCloudWatchConf) (ri : RuntimeInfo) (inbox : IActor<_>) =

    let add (state : State) (metric : Measure) =
      let key, newDatum = (metric.Name, metric.Type), metric.ToMetricDatum()
      match metricData.TryGetValue key with
      | true, datum -> combine datum newDatum
      | _ -> metricData.[key] <- newDatum
      
    let pub client =
      metricData.Values
      |> Seq.groupsOfAtMost Constants.putMetricDataListSize
      |> sendAll conf.rootNamespace client
      |> Async.StartAsPlainTask
      |> (fun task -> task.Wait())
      metricData.Clear()

    let rec init () = async {
      let c = conf.awsCreds
      let client = new AmazonCloudWatchClient(c.accessKeyId, c.secretAccessKey, c.region)
      return! loop
        { metricData = Map.empty
          client     = client }
      }
    and loop state = async {
      let! msg, _ = inbox.Receive()
      match msg with
      | Log l ->
        return! loop state
      | Measure msr ->
        let state' = add state msr
        return! loop state'
      | Flush ackChan ->
        pub state.client () // TODO: separate into one reservoir, one target
        ackChan.Reply Ack
        return! loop state
      | Shutdown ackChan ->
        ackChan.Reply Ack
        return ()
      }

    init ()

/// Create a new AWSCloudWatch target
let create conf = TargetUtils.stdNamedTarget (Impl.loop conf)

/// C# Interop: Create a new AWSCloudWatch target
[<CompiledName "Create">]
let create' (conf, name) =
  create conf name

type SecondStep =
  abstract Credentials : string * string * string -> FactoryApi.TargetConfBuild<Builder>

/// Use with LogaryFactory.New( s => s.Target<Noop.Builder>() )
and Builder(conf, callParent : FactoryApi.ParentCallback<Builder>) =
  member x.RootNamespace(ns : string) =
    Builder({ conf with rootNamespace = ns }, callParent)
    :> SecondStep

  interface SecondStep with
    /// Specify the credentials for your AWS Account
    member x.Credentials (awsAccessKeyId, awsSecretAccessKey, region) =
      let c =
        { accessKeyId     = awsAccessKeyId
          secretAccessKey = awsSecretAccessKey
          region          = region }
      ! (callParent <| Builder({ conf with awsCreds = c }, callParent))

  new(callParent : FactoryApi.ParentCallback<_>) =
    Builder(empty, callParent)

  interface Logary.Target.FactoryApi.SpecificTargetConf with
    member x.Build name = create conf name
