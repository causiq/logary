module Logary.Tests.Parsing

open System
open System.Collections.Generic

open Fuchu
open NodaTime

open Logary.TOML

let example = """
[group1]
key = true
key2 = 1337
title = "TOML Example"

[  owner]
name = "Tom Preston-Werner"
organization = "GitHub"
bio = "GitHub Cofounder & CEO\nLikes tater tots and beer."
dob =  1979-05-27T07:32:00Z   # First class dates? Why not?

[database  ]
server= "192.168.1.1"
ports       =  [ 8001,8001 , 8002]
connection_max =5000
enabled=true

[servers]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  [  servers  .alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.  beta  ]
  ip = "10.0.0.2"
  dc = "eqdc10"

[clients]
data = [ ["gamma","delta"], [1, 2] ] # just an update to make sure parsers support it

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
  ]
"""


// specific to parser:
let dic = Parser.parse example
let value key = dic.[key] :?> 'a // if you type parser, change this f-n

// helper:
let case msg key expected =
  testCase msg (fun () ->
    try
      let value' = value key
      Assert.Equal(key + ": " + msg, expected, value')
    with
    | :? InvalidCastException ->
      Tests.failtestf "cast to %O failed, type: %O"
        (expected.GetType())
        ((value key : obj).GetType()))

[<Tests>]
let basicExample =
  testList "parsing contents" [
    yield testCase "no crash" <| fun _ ->
      Parser.parse example |> ignore

    yield case "should be true" "group1.key" true
    yield case "should be 1337" "group1.key2" 1337L
    yield case "should have title" "group1.title" "TOML Example"
    yield case "should have owner name" "owner.name" "Tom Preston-Werner"
    yield case "should have owner org" "owner.organization" "GitHub"
    yield case "should have owner bio" "owner.bio" "GitHub Cofounder & CEO\nLikes tater tots and beer."
    yield case "should have owner dob" "owner.dob" (Instant.FromUtc(1979, 05, 27, 7, 32))
    yield case "should have database server" "database.server" "192.168.1.1"
    yield case "should have database ports" "database.ports" [ 8001L; 8001L; 8002L ]
    yield case "should have database connection_max" "database.connection_max" 5000L
    yield case "should have enabled" "database.enabled" true
    yield case "servers 1 ip" "servers.alpha.ip" "10.0.0.1"
    yield case "servers 1 dc" "servers.alpha.dc" "eqdc10"
    yield case "servers 2 ip" "servers.beta.ip" "10.0.0.2"
    yield case "servers 2 ip" "servers.beta.dc" "eqdc10"
    yield case "clients" "clients.data" [ box [box "gamma"; box "delta"]; box [box 1L; box 2L] ]
    yield case "hosts" "clients.hosts" [ "alpha"; "omega" ]
    ]

let tableArr = """
[[products]]
name = "Hammer"
sku = 738594937

[[products]]

[[products]]
name = "Nail"
sku = 284758393
color = "gray"
"""

let hardExample = """
# Test file for TOML
# Only this one tries to emulate a TOML file written by a user of the kind of parser writers probably hate
# This part you'll really hate

[the]
test_string = "You'll hate me after this - #"          # " Annoying, isn't it?

    [the.hard]
    test_array = [ "] ", " # "]      # ] There you go, parse this!
    test_array2 = [ "Test #11 ]proved that", "Experiment #9 was a success" ]
    # You didn't think it'd as easy as chucking out the last #, did you?
    another_test_string = " Same thing, but with a string #"
    harder_test_string = " And when \"'s are in the string, along with # \""   # "and comments are there too"
    # Things will get harder
    
        [the.hard.bit#]
        what? = "You don't think some user won't do that?"
        multi_line_array = [
            "]",
            # ] Oh yes I did
            ]

# Each of the following keygroups/key value pairs should produce an error. Uncomment to them to test

#[error]   if you didn't catch this, your parser is broken
#string = "Anything other than tabs, spaces and newline after a keygroup or key value pair has ended should produce an error unless it is a comment"   like this
#array = [
#         "This might most likely happen in multiline arrays",
#         Like here,
#         "or here,
#         and here"
#         ]     End of array comment, forgot the #
#number = 3.14  pi <--again forgot the #         
"""

[<Tests>]
let advancedExamples =
  testList "advanced" [
    testCase "can parse array" <| fun _ ->
      Parser.parse tableArr |> ignore

    testCase "products" <| fun _ ->
      let dic = Parser.parse tableArr
      Assert.NotNull("should have products list", dic.["products"])
      let exp =
        Dictionary([ "name", box "Hammer"
                     "sku",  box 738594937L ] |> Map.ofList) :> IDictionary<string, obj>
        :: upcast Dictionary()
        :: upcast Dictionary([ "name",  box "Nail"
                               "sku",   box 284758393L
                               "color", box "gray"] |> Map.ofList)
        :: []
      Assert.Equal("should have products list", box exp, dic.["products"])
    ]

// specific to parser:
let dic' = Parser.parse hardExample
let value' key = dic'.[key] :?> 'a // if you type parser, change this f-n

// helper:
let case' msg key expected =
  testCase msg (fun () ->
    try
      let value'' = value' key
      Assert.Equal(key + ": " + msg, expected, value'')
    with
    | :? InvalidCastException ->
      Tests.failtestf "cast to %O failed, type: %O"
        (expected.GetType())
        ((value' key : obj).GetType())
    | :? KeyNotFoundException ->
      let data = dic' |> Seq.fold (fun s t -> s + sprintf "%s: %A\n" t.Key t.Value) ""
      Tests.failtestf "couldn't find key %s\ndata:\n%s" key data)

[<Tests>]
let hardTests =
  testList "hard example" [
    testCase "can parse" <| fun _ ->
      Parser.parse hardExample |> ignore

    case' "the.test_string" "the.test_string" "You'll hate me after this - #"
    case' "parsing array with symbols" "the.hard.test_array" [ "] "; " # "]
    case' "parsing array with symbols 2" "the.hard.test_array2" [ "Test #11 ]proved that"; "Experiment #9 was a success"]
    case' "parsing string with comment char" "the.hard.another_test_string" "Same thing, but with a string #"
    case' "parsing string with escapes" "the.hard.harder_test_string" " And when \"'s are in the string, along with # \""
    case' "parsing funky key" "the.hard.bit#.what?" "You don't think some user won't do that?"
    case' "parsing multi line array" "the.hard.bit#.multi_line_array" ["]"]
    ]