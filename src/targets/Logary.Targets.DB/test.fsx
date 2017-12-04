#I "bin/Debug"

#r "NodaTime.dll"
#r "Hopac.dll"
#r "Hopac.Core.dll"
#r "Logary.dll"
#r "FsPickler.dll"
#r "FsPickler.Json.dll"
#r "Newtonsoft.Json.dll"

open Logary
open System
open Newtonsoft.Json
open MBrace.FsPickler.Json
open MBrace.FsPickler
open MBrace.FsPickler.Combinators

type User = 
  {
    id      : int 
    name    : string
    created : DateTime
  }
with
  interface IFormattable with
    member x.ToString (format, provider) =
      sprintf "my id is %i and my name is %s, created => %s" x.id x.name (x.created.ToShortDateString())

type Obj() =
  member x.PropA =
    45
  member x.PropB =
    "Oh noes, no referential transparency here"
with
  interface IFormattable with
    member x.ToString (format, provider) = "PropA is 45 and PropB raise exn"

let date20171111 =  DateTime.Parse("2017-11-11")
let foo = { id = 999; name = "whatever"; created = date20171111}

let msg : Message =
  let ex = exn "exception with data in it"
  ex.Data.Add ("data 1 in exn", 1)
  ex.Data.Add ("data foo in exn", foo)
  ex.Data.Add (foo, foo)

  let tp = (1, "two", foo)
  let object = Obj ()
  let scalarArr = [| box 1;box 2;box 3; box "4";box "5";box 6.0; box date20171111 |]
  let notScalarList = [box foo; box tp]
  let scalarKeyValueMap = [ 1,"one" ; 2, "two"] |> HashMap.ofSeq
  let scalarKeyMap = Map [ "some user", box foo ; "some obj", box object]
  let notScalarMap = Map [([2,"2"],["3";"4"]); ([1,"a";2,"b"],["hello";"world"])]

  Message.eventFormat (Info, 
    "default foo is {foo} here is a default {objDefault} and stringify {$objStr} and destructure {@objDestr}", 
    [| foo; object; object; object; |])
  |> Message.setName  (PointName.ofList ["a"; "b"; "c"; "d"])
  |> Message.setNanoEpoch 3123456700L
  |> Message.setContext "UserInfo" foo
  |> Message.setContext "Some Tuple With 1 two foo" tp
  |> Message.setContext "scalar array" scalarArr
  |> Message.setContext "no scalar list" notScalarList
  |> Message.setContext "simple scalar key/value map" scalarKeyValueMap
  |> Message.setContext "just scalar key map" scalarKeyMap
  |> Message.setContext "no scalar key/value map" notScalarMap
  |> Message.addGauge "svc1 request per second" (Gauge(1750., Units.Scalar))
  |> Message.addGauge "Processor.% Idle.Core 1" (Gauge(0.75, Units.Percent))
  |> Message.addGauge "methodA" (Gauge(25000000000., Units.Scaled (Seconds, float Constants.NanosPerSecond)))
  |> Message.addExn ex
  |> Message.addExn (exn "another exception")

let jsonStr = JsonConvert.SerializeObject msg
jsonStr

let ctxPickler (resolver : IPicklerResolver) =

    let writer (w : WriteState) (ctx : list<string * obj>) =
      ctx 
      |> List.sortBy fst
      |> List.iter (fun (name, v) ->
         let p = resolver.Resolve (v.GetType())
         let t = v.GetType()
         let methodInfo = typeof<Pickler>.GetMethod("UntypedWrite")
         methodInfo.Invoke(p, [|w; name; v|]) |> ignore
         )
     
    let reader (r : ReadState) =
        failwith "unsupport"

    Pickler.FromPrimitives(reader, writer)

let msgPickler (resolver : IPicklerResolver) =

    let writer (w : WriteState) (msg : Message) =
        Pickler.auto.Write w "name" (string msg.name)
        Pickler.auto.Write w "level" (string msg.level)
        Pickler.auto.Write w "timestamp" (msg.timestamp)
        let (Event tpl) = msg.value
        Pickler.auto.Write w "Event" (tpl)
        (resolver.Resolve<list<string * obj>> ()).Write w "Context" (msg.context |> HashMap.toList)

    let reader (r : ReadState) =
        failwith "unsupport"

    Pickler.FromPrimitives(reader, writer)



let registry = new CustomPicklerRegistry()
do registry.RegisterFactory ctxPickler
do registry.RegisterFactory msgPickler

// 2. Construct a new pickler cache
let cache = PicklerCache.FromCustomPicklerRegistry registry

// 3. Pass the new cache to a serializer instance
let jsonSerializer = FsPickler.CreateJsonSerializer(indent = false, omitHeader = true,picklerResolver = cache)
// let jsonSerializer = FsPickler.CreateJsonSerializer(indent = false, omitHeader = true)


let picklerStr = jsonSerializer.PickleToString msg
picklerStr
// let msgUnPickler = jsonSerializer.UnPickleOfString<Message> picklerStr
// msgUnPickler.context


// jsonSerializer.PickleToString (Obj())
// jsonSerializer.PickleToString (exn "123" |> box)
// JsonConvert.SerializeObject (exn "23")