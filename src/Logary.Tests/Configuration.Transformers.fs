module Logary.Tests.Transformers

open Expecto
open NodaTime
open Hopac
open Logary
open Logary.Configuration
open Logary.Configuration.Transformers

let bufferCounter =
  let take n stream =
    stream |> Stream.take n |> Stream.toSeq

  let generateBufferTicker () = job {
    let bufferTicker = BufferTicker ()
    let expects =  Stream.Src.create ()
    let! (sendItem, _) =
      Pipe.start
      |> Pipe.tick bufferTicker
      |> Pipe.run (fun items -> Stream.Src.value expects items |> HasResult)

    return (sendItem, bufferTicker, Stream.Src.tap expects)
  }

  testList "simplest counter" [
    yield testCaseJob "initial" (job {
      let! (sendItem, ticker, expects) = generateBufferTicker ()
      do! ticker.tick ()
      let! expect = expects |> take 1L
      let expect = expect |> Seq.toList |> List.map List.ofSeq
      Expect.equal expect [[]] "emptry without sendItem"
    })

    yield testCaseJob "counting to three" (job {
      let! (sendItem, ticker, expects) = generateBufferTicker ()
      do! sendItem 1 |> PipeResult.defaultValue (Job.unit ())
      do! ticker.tick ()
      do! sendItem 1  |> PipeResult.defaultValue (Job.unit ())
      do! sendItem 1  |> PipeResult.defaultValue (Job.unit ())
      do! timeOutMillis 100 // since senditem is async, so the order is not guarantee   // Mailbox.Now.send updateMb prev
      do! ticker.tick ()
      let! expect = expects |> take 2L
      let expect = expect |> Seq.toList
      Expect.equal expect [[1];[1;1;];] "one and then two after two single counts"
    })
  ]

let snapshot =
  let sample = Snapshot.create [| 5L; 1L; 2L; 3L; 4L |]
  let empty = Snapshot.create [||]

  testList "calculating snapshot values" [
    testCase "small quantiles are first value" <| fun _ ->
      Expect.floatClose
        Accuracy.veryHigh (Snapshot.quantile sample 0.) 1.
        "should be the one"

    testCase "big quantiles are last values" <| fun _ ->
      Expect.floatClose
        Accuracy.veryHigh (Snapshot.quantile sample 1.) 5.
        "should be the five"

    testCase "median" <| fun _ ->
      Expect.floatClose Accuracy.veryHigh (Snapshot.median sample) 3.
                        "should have median"

    testCase "75th percentile" <| fun _ ->
      Expect.floatClose Accuracy.veryHigh (Snapshot.percentile75th sample) 4.5
                        "should have 75th percentile"

    testCase "95th percentile" <| fun _ ->
      Expect.floatClose Accuracy.veryHigh (Snapshot.percentile95th sample) 5.
                        "should have 95th percentile"

    testCase "98th percentile" <| fun _ ->
      Expect.floatClose Accuracy.veryHigh (Snapshot.percentile98th sample) 5.
                        "should have 98th percentile"

    testCase "99th percentile" <| fun _ ->
      Expect.floatClose Accuracy.veryHigh (Snapshot.percentile99th sample) 5.
                        "should have 99th percentile"

    testCase "999th percentile" <| fun _ ->
      Expect.floatClose Accuracy.veryHigh (Snapshot.percentile999th sample) 5.
                        "should have 999th percentile"

    testCase "has values" <| fun _ ->
      Expect.equal (Snapshot.values sample) [| 1L; 2L; 3L; 4L; 5L |]
                   "should have values ordered"

    testCase "has size" <| fun _ ->
      Expect.equal (Snapshot.size sample) 5 "should have five size"

    testCase "has mimimum value" <| fun _ ->
      Expect.equal (Snapshot.min sample) 1L "has a mimimum value"

    testCase "has maximum value" <| fun _ ->
      Expect.equal (Snapshot.max sample) 5L "has a maximum value"

    testCase "has mean value" <| fun _ ->
      Expect.equal (Snapshot.mean sample) 3. "has a mean value"

    testCase "has stdDev" <| fun _ ->
      Expect.floatClose Accuracy.low (Snapshot.stdDev sample) 1.5811 "has stdDev"

    testCase "empty: min" <| fun _ ->
      Expect.equal (Snapshot.min empty) 0L "zero"

    testCase "empty: max" <| fun _ ->
      Expect.equal (Snapshot.max empty) 0L "zero"

    testCase "empty: mean" <| fun _ ->
      Expect.floatClose Accuracy.veryHigh (Snapshot.mean empty) 0. "zero"

    testCase "empty: std dev" <| fun _ ->
      Expect.floatClose Accuracy.veryHigh (Snapshot.mean empty) 0. "zero"
  ]

let mockClock () =
  let mutable num = 0.
  let firstInstant = lazy (SystemClock.Instance.GetCurrentInstant())
  { new IClock with
      member x.GetCurrentInstant () =
        let res = firstInstant.Value + Duration.FromSeconds(5. * num)
        num <- num + 1.
        res
  }

let reservoirs =
  testList "reservoirs" [
    testCase "uniform: update 1000 times" <| fun _ ->
      let state =
        [ 0L .. 999L ]
        |> List.fold Uniform.update (Uniform.create 100)
      Expect.equal (Uniform.size state) 100 "should have 100L size"

      let snap = Uniform.snapshot state
      Expect.equal (Snapshot.size snap) 100 "snapshot has as many"
      for v in snap.values do
        Expect.isTrue (0L <= v && v <= 999L)
                      (sprintf "'%d' should be in [0, 999]" v)

    testCase "sliding: small" <| fun _ ->
      let state =
        [ 1L; 2L ]
        |> List.fold SlidingWindow.update (SlidingWindow.create 3)
      let snap = SlidingWindow.snapshot state
      Expect.equal state.count 2I "has two"
      Expect.equal (SlidingWindow.size state) 2 "size has two"
      Expect.equal (snap.values.Length) 2 "snap has two"
      Expect.equal (Snapshot.values snap) [| 1L; 2L; |] "should have correct order"

    testCase "sliding: only last values" <| fun _ ->
      let state =
        [ 1L..5L ]
        |> List.fold SlidingWindow.update (SlidingWindow.create 3)
      let snap = SlidingWindow.snapshot state
      Expect.equal (Snapshot.values snap) [| 3L..5L |] "should have correct order"

    testList "sliding time window" [
      testCase "store duplicate ticks" <| fun _ ->
        skiptest "TODO: might port sliding time window reservoir"
      ]

    testList "exponentially weighted moving average" [
      let testEWMA explaination instance (expectations: _ list) =

        let flip f a b = f b a
        let passMinute s = // 5 second sampling rate, see mockClock
          [ 1..12 ] |> List.fold (fun s' t -> ExpWeightedMovAvg.tick s') s

        let initState =
          instance |> (flip ExpWeightedMovAvg.update) 3L |> ExpWeightedMovAvg.tick |> ExpWeightedMovAvg.tick
        let actual =
          [ for i in 1..expectations.Length - 1 do yield i ]
          |> List.scan (fun s t -> passMinute s) initState
          |> List.map (ExpWeightedMovAvg.rateInUnit (Duration.FromSeconds 1L))

        testCase explaination <| fun _ ->
          List.zip expectations actual
          |> List.iteri (fun index (expected, actual) ->
             Expect.floatClose Accuracy.medium actual expected
                              (sprintf "Index %d, should calculate correct EWMA" index))

      yield testEWMA "1 min"
        (ExpWeightedMovAvg.oneMinuteEWMA (mockClock()))
        [ 0.6
          0.22072766
          0.08120117
          0.02987224
          0.01098938
          0.00404277
          0.00148725
          0.00054713
          0.00020128
          0.00007405
          0.00002724
          0.00001002
          0.00000369
          0.00000136
          0.00000050
          0.00000018 ]

      yield testEWMA "5 min"
        (ExpWeightedMovAvg.fiveMinutesEWMA (mockClock()))
        [ 0.6
          0.49123845
          0.40219203
          0.32928698
          0.26959738
          0.22072766
          0.18071653
          0.14795818
          0.12113791
          0.09917933
          0.08120117
          0.06648190
          0.05443077
          0.04456415
          0.03648604
          0.02987224 ]

      yield testEWMA "15 min"
        (ExpWeightedMovAvg.fifteenMinuteEWMA (mockClock()))
        [ 0.6
          0.56130419
          0.52510399
          0.49123845
          0.45955700
          0.42991879
          0.40219203
          0.37625345
          0.35198773
          0.32928698
          0.30805027
          0.28818318
          0.26959738
          0.25221023
          0.23594443
          0.22072766 ]
      ]
   ]

let tests = [
  bufferCounter
  snapshot
  reservoirs
]
