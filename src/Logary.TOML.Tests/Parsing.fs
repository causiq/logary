module Parsing

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

open System

open Fuchu
open NodaTime

open Logary

// specific to parser:
let dic = TOML.parse example
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
      TOML.parse example |> ignore

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

[<Tests>]
let advancedExamples =
  testList "advanced" [
    testCase "can parse array" <| fun _ ->
      TOML.parse tableArr |> ignore
    testCase "products" <| fun _ ->
      let dic = TOML.parse tableArr
      Assert.NotNull("should have products array", dic.["products[0]"])
    ]