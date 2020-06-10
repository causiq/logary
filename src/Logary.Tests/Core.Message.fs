module Logary.Tests.Message

open System
open Hopac
open Expecto
open FsCheck
open Logary
open Logary.Message

let tests =
  [
      testCase "event: LogLevel -> string -> Message" <| fun _ ->
        let m = event Info "Hello world"
        Expect.equal m.level Info "Should have info level"
        Expect.equal m.value ("Hello world") "Should have template"

      testCase "Model.EventMessage: string -> LogLevel -> Message" <| fun _ ->
        let m = Model.EventMessage "Hello world" Info
        Expect.equal m.level Info "Should have info level"
        Expect.equal m.value ("Hello world") "Should have template"

      testProperty "tryGetContext after setContext" <| fun name ->
        eventInfo "" |> setContext name name |> tryGetContext name
        |> function | v when isNull name -> Option.isNone v | v -> v = (Some name)

      testCase "setContext override value when name exist" <| fun _ ->
        let get = tryGetContext "key" >> Option.get
        let set v = setContext "key" v
        let m1 = eventInfo "" |> set 1
        Expect.equal (get m1) 1 "Should properly set context value"
        let m2 = m1 |> set 2
        Expect.equal (get m2) 2 "Should override"

      testProperty "setField should have field prefix" <| fun name ->
        let prefixName = KnownLiterals.FieldsPrefix + name
        let fv = eventInfo "" |> setField name 1 |> tryGetContext prefixName
        Expect.isSome fv "Value should be found"
        Expect.equal fv.Value 1 "Value should be found"

      testProperty "tryGetField" <| fun name ->
        let msg = eventInfo ""
        let value = msg |> tryGetField name
        Expect.isNone value "Should not be found"
        let value = msg |> setField name name |> tryGetField name
        Expect.equal value (Some name) "Should be same"

      testCase "getAllFields" <| fun _ ->
        let names = Arb.generate<NonEmptyString> |> Gen.sample 0 5 |> List.distinct |> List.map (fun (NonEmptyString name) -> name)
        names
        |> List.fold (fun m name ->
           m |> setField name name) (eventInfo "")
        |> getAllFields |> Seq.length |> fun c ->
           Expect.equal c names.Length "Should get same length after set fields"

      testPropertyWithConfig fsc "gaugeMessage" <| fun g ->
        let saved =
          gaugeWithUnit (PointName.parse "Car.Speedometer") "dv/dt" g
          |> tryGetGauge "dv/dt"
        Expect.equal saved (Some g) "Should be same"

      testCase "getAllGauges" <| fun _ ->
        let names = Arb.generate<NonEmptyString> |> Gen.sample 0 5 |> List.distinct |> List.map (fun (NonEmptyString name) -> name)
        let msg = names |> List.fold (fun m name -> m |> addGauge name (Gauge (Value.Float 1., Units.Scalar))) (eventInfo "")
        let c = msg |> getAllGauges |> Seq.length
        Expect.equal c names.Length "Should get same length after add gauges"

      testCase "getAllTags & hasTag" <| fun _ ->
        let tags = Arb.generate<NonEmptyString> |> Gen.sample 0 5 |> List.distinct |> List.map (fun (NonEmptyString name) -> name)
        let msg = tags |> List.fold (fun m name -> m |> tag name) (eventInfo "")
        msg |> getAllTags |> fun tagSet ->
           Expect.equal tagSet.Count tags.Length "Should get same length after add tags"
           Expect.equal (tags |> Set.ofList) tagSet "Should be same set"
           Expect.containsAll tagSet tags "Should contains all tags"
        let msgHasAllTag = tags |> List.forall (fun tag -> hasTag tag msg )
        Expect.isTrue msgHasAllTag "Should have all tags"

      testCase "setFieldsFromObject: obj -> Message -> Message" <| fun () ->
        let m = Model.EventMessage "Hello world" Info |> setFieldsFromObject (SampleObject())
        let field = m |> tryGetField "PropA"
        Expect.equal field (Some 45) "Should have PropA"

      testCase "eventFormat" <| fun _ ->
        let m = Message.eventFormat (Info, "some {@data} created at {$time}", 1,2)
        let dataf = m |> tryGetField "data"
        let timef = m |> tryGetField "time"
        Expect.equal dataf (Some 1) "Should have data field"
        Expect.equal timef (Some 2) "Should have time field"

      testCase "addExn & getExns" <| fun _ ->
        let e1 = Exception ("e1")
        let e2 = ArgumentNullException ("e2")
        let errors = eventInfo "" |> addExn e1 |> addExn e2 |> getExns
        Expect.contains errors e1 "Should have exn"
        Expect.contains errors (upcast e2) "Should have arg null exn"

      testCase "time and timeJob" <| fun _ ->
        let name = PointName.parse "some.gauge.at.location"
        let timeFun = time name id
        let res, msg = timeFun 100
        Expect.equal res 100 "Should have result"

        let g = tryGetGauge "time" msg
        Expect.isSome g "Should have guage"
        match g with
        | Some (Gauge (_, Units.Scaled (Seconds, _))) -> ()
        | g -> failtestf "Should have units.scaled (seconds, _) , actual: %A" g

      testCaseAsync "time & timeJob" <| (async {
        let name = "some.gauge.at.location"

        let test res msg =
          Expect.equal res 100 "Should have result"
          let g = tryGetGauge "time" msg
          Expect.isSome g "Should have gauge"
          match g with
          | Some (Gauge (_, Units.Scaled (Seconds, _))) -> ()
          | g -> failtestf "Should have units.scaled (seconds, _) , actual: %A" g

        let timeFun = time (PointName.parse name) id
        let timeJobFun = timeJob (PointName.parse name) Job.result
        let! res1, msg1 = timeJobFun 100 |> Job.toAsync
        let res2, msg2 = timeFun 100
        test res1 msg1
        test res2 msg2
      })
  ]