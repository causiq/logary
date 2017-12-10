module FormattingFromFsMessageTemplates

open System
open System.Globalization
open Expecto
open Logary.MessageTemplates

let invariantProvider = (System.Globalization.CultureInfo.InvariantCulture :> System.IFormatProvider)
type MtAssert =
  static member RenderedAs(template, values, expected, ?provider, ?maxDepth) =
    testCase (Guid.NewGuid().ToString()) <| fun _ ->
      let provider = defaultArg provider invariantProvider
      let actual = Formatting.formatWithProvider provider template values
      Expect.equal actual expected "format should work"


type Chair() =
    member __.Back with get() = "straight"
    member __.Legs with get() = [|1;2;3;4|]
    override __.ToString() = "a chair"

type Receipt() =
    member __.Sum with get() = 12.345m
    member __.When with get() = System.DateTime(2013, 5, 20, 16, 39, 0)
    override __.ToString() = "a receipt"

// Delegate1 works with tuple arguments. 
type MyDelegate = delegate of (int * int) -> int


[<Tests>]
let``a delegate is rendered as a string`` =
    let myDel = MyDelegate(fun (i1,i2) -> i1+i2)
    MtAssert.RenderedAs( "What even would a {del} print?", [| myDel |],
        """What even would a "FormattingFromFsMessageTemplates+MyDelegate" print?""")


[<Tests>]
let``a class instance is rendered in simple notation`` =
    MtAssert.RenderedAs( "I sat at {@Chair}", [|Chair()|],
        "I sat at Chair { Back: \"straight\", Legs: [1, 2, 3, 4] }")


[<Tests>]
let``a class instance is rendered in simple notation using format provider`` =
    MtAssert.RenderedAs( "I received {@Receipt}", [|Receipt()|],
        "I received Receipt { Sum: 12,345, When: 20/05/2013 16:39:00 }",
        provider=CultureInfo("fr-FR"))

type ChairRecord = { Back:string; Legs: int array }


[<Tests>]
let``an F# record object is rendered in simple notation with type`` =
    MtAssert.RenderedAs( "I sat at {@Chair}", [|{ Back="straight"; Legs=[|1;2;3;4|] }|],
        "I sat at ChairRecord { Back: \"straight\", Legs: [1, 2, 3, 4] }")

type ReceiptRecord = { Sum: double; When: System.DateTime }


[<Tests>]
let``an F# record object is rendered in simple notation with type using format provider`` =
    MtAssert.RenderedAs( "I received {@Receipt}",
        [| { Sum=12.345; When=DateTime(2013, 5, 20, 16, 39, 0) } |],
        "I received ReceiptRecord { Sum: 12,345, When: 20/05/2013 16:39:00 }",
        provider=(CultureInfo("fr-FR")))


[<Tests>]
let``an object with default destructuring is rendered as a string literal`` =
    MtAssert.RenderedAs( "I sat at {Chair}", [|Chair()|], "I sat at \"a chair\"")


[<Tests>]
let``an object with stringify destructuring is rendered as a string`` =
    MtAssert.RenderedAs( "I sat at {$Chair}", [|Chair()|], "I sat at \"a chair\"")


[<Tests>]
let``multiple properties are rendered in order`` =
    MtAssert.RenderedAs( "Just biting {Fruit} number {Count}", [| "Apple"; 12 |],
        "Just biting \"Apple\" number 12")
    

[<Tests>]
let``a template with only positional properties is analyzed and rendered positionally`` =
    MtAssert.RenderedAs( "{1}, {0}", [|"world"; "Hello"|], "\"Hello\", \"world\"")


[<Tests>]
let``a template with only positional properties uses format provider`` =
    MtAssert.RenderedAs( "{1}, {0}", [| 12.345; "Hello" |], "\"Hello\", 12,345",
        provider=(CultureInfo("fr-FR")))

// Debatable what the behavior should be, here.

[<Tests>]
let``a template with names and positionals uses names for all values`` =
    MtAssert.RenderedAs( "{1}, {Place}, {5}" , [|"world"; "Hello"; "yikes"|],
        "\"world\", \"Hello\", \"yikes\"")


[<Tests>]
let``missing positional parameters render as text like standard formats`` =
    MtAssert.RenderedAs( "{1}, {0}", [|"world"|], "{1}, \"world\"")


[<Tests>]
let``extra positional parameters are ignored`` =
    MtAssert.RenderedAs( "{1}, {0}", [|"world"; "world"; "world"|], "\"world\", \"world\"")
    

[<Tests>]
let``the same positional parameter repeated many times with literal format is reused`` =
    MtAssert.RenderedAs( "{1:l}{1:l}{1:l}{0}{1:l}{1:l}{1:l}", [|"a";"b"|], "bbb\"a\"bbb")
    

[<Tests>]
let``the same missing positional parameters render literally`` =
    MtAssert.RenderedAs( "{1}{2}{3}{4}{5}{6}{7}{8}{9}", [|"a"|], "{1}{2}{3}{4}{5}{6}{7}{8}{9}")
    MtAssert.RenderedAs( "{1}{2}{3}{4}{5}{0}{6}{7}{8}{9}", [|"a"|], "{1}{2}{3}{4}{5}\"a\"{6}{7}{8}{9}")
    

[<Tests>]
let``multiple properties use format provider`` =
    MtAssert.RenderedAs( "Income was {Income} at {Date:d}", [| 1234.567; DateTime(2013, 5, 20) |],
        "Income was 1234,567 at 20/05/2013", provider=(CultureInfo("fr-FR")))


[<Tests>]
let``format strings are propagated`` =
    MtAssert.RenderedAs( "Welcome, customer {CustomerId:0000}", [|12|],
        "Welcome, customer 0012")

type Cust() =
    let c = Chair()
    member __.Seat = c
    member __.Number = 1234
    override __.ToString() = "1234"

[<Tests>]
let``get alignment structure values`` () : obj[] seq = seq {
    let values : obj[] = [| 1234 |]
    yield [| "C#"; values; "cus #{CustomerId,-10}, pleasure to see you";        "cus #1234      , pleasure to see you" |]
    yield [| "C#"; values; "cus #{CustomerId,-10}, pleasure to see you";        "cus #1234      , pleasure to see you" |]
    yield [| "C#"; values; "cus #{CustomerId,-10:000000}, pleasure to see you"; "cus #001234    , pleasure to see you" |]
    yield [| "C#"; values; "cus #{CustomerId,10}, pleasure to see you";         "cus #      1234, pleasure to see you" |]
    yield [| "C#"; values; "cus #{CustomerId,10:000000}, pleasure to see you";  "cus #    001234, pleasure to see you" |]
    yield [| "C#"; values; "cus #{CustomerId,10:0,0}, pleasure to see you";     "cus #     1,234, pleasure to see you" |]
    yield [| "C#"; values; "cus #{CustomerId:0,0}, pleasure to see you";        "cus #1,234, pleasure to see you"      |]
    yield [| "F#"; values; "cus #{CustomerId,-10}, pleasure to see you";        "cus #1234      , pleasure to see you" |]
    yield [| "F#"; values; "cus #{CustomerId,-10:000000}, pleasure to see you"; "cus #001234    , pleasure to see you" |]
    yield [| "F#"; values; "cus #{CustomerId,10}, pleasure to see you";         "cus #      1234, pleasure to see you" |]
    yield [| "F#"; values; "cus #{CustomerId,10:000000}, pleasure to see you";  "cus #    001234, pleasure to see you" |]
    yield [| "F#"; values; "cus #{CustomerId,10:0,0}, pleasure to see you";     "cus #     1,234, pleasure to see you" |]
    yield [| "F#"; values; "cus #{CustomerId:0,0}, pleasure to see you";        "cus #1,234, pleasure to see you"      |]
    
    let values : obj[] = [| Cust() |]
    yield [| "C#"; values; "cus #{$cust:0,0}, pleasure to see you";             "cus #\"1234\", pleasure to see you"    |]
    yield [| "F#"; values; "cus #{$cust:0,0}, pleasure to see you";             "cus #\"1234\", pleasure to see you"    |]
    // formats/alignments don't propagate through to the 'destructured' inside values
    // They only apply to the outer (fully rendered) property text
    yield [| "C#"; values; "cus #{@cust,80:0,0}, pleasure to see you";          "cus #     Cust { Seat: Chair { Back: \"straight\", Legs: [1, 2, 3, 4] }, Number: 1234 }, pleasure to see you"    |]
    yield [| "F#"; values; "cus #{@cust,80:0,0}, pleasure to see you";          "cus #     Cust { Seat: Chair { Back: \"straight\", Legs: [1, 2, 3, 4] }, Number: 1234 }, pleasure to see you"    |]
}

// [<LangTheory; MemberData("get alignment structure values")>]
[<Tests>]
let``alignment strings are propagated`` =
  ``get alignment structure values`` ()
  |> Seq.map (fun objs ->
     MtAssert.RenderedAs(downcast objs.[2], downcast objs.[1], downcast objs.[3]))
  |> fun tests ->
     testList "alignment strings are propagated"



[<Tests>]
let``format provider is used`` =
    MtAssert.RenderedAs(
        "Please pay {Sum}", [|12.345|], "Please pay 12,345",
        provider=(CultureInfo("fr-FR")))

type Tree = Seq of nums: double list | Leaf of double | Trunk of double * DateTimeOffset * (Tree list)
type ItemsUnion = ChairItem of c:ChairRecord | ReceiptItem of r:ReceiptRecord


[<Tests>]
let``an F# discriminated union object is formatted with provider correctly`` =
    let provider = CultureInfo("fr-FR")
    let template = "I like {@item1} and {@item2}"
    let values : obj[] = [| ChairItem({ Back="straight"; Legs=[|1;2;3;4|] })
                            ReceiptItem({ Sum=12.345; When=DateTime(2013, 5, 20, 16, 39, 0) }) |]
    let expected = """I like ("ChairItem": ChairRecord { Back: "straight", Legs: [1, 2, 3, 4] }) and ("ReceiptItem": ReceiptRecord { Sum: 12,345, When: 20/05/2013 16:39:00 })"""
    MtAssert.RenderedAs( template, values, expected, provider)


[<Tests>]
let``Rendered F# DU or Tuple fields are 'null' when depth is 1`` =
    let provider = (CultureInfo("fr-FR"))
    let template = "I like {@item1} and {@item2} and {@item3}"
    let values : obj[] = [| Leaf 12.345
                            Leaf 12.345
                            Trunk (12.345, DateTimeOffset(2013, 5, 20, 16, 39, 00, TimeSpan.FromHours 9.5), [Leaf 12.345; Leaf 12.345]) |]
    // all fields should be rendered
    let expected = """I like ("Leaf": 12,345) and ("Leaf": 12,345) and ("Trunk": [12,345, 20/05/2013 16:39:00 +09:30, [("Leaf": 12,345), ("Leaf": 12,345)]])"""
    
    MtAssert.RenderedAs( template, values, expected, provider, maxDepth=1)


[<Tests>]
let``Rendered F# DU or Tuple fields on level3 are 'null' when depth is 2`` =
    let provider = (CultureInfo("fr-FR"))
    let template = "I like {@item1} and {@item2} and {@item3} and {@item4}"
    let values : obj[] = [| Leaf 12.345
                            Leaf 12.345
                            Trunk (12.345, DateTimeOffset(2013, 5, 20, 16, 39, 00, TimeSpan.FromHours 9.5), [Leaf 12.345; Leaf 12.345])
                            ChairItem { Back="slanted"; Legs=[|1;2;3;4;5|] } |]

    // Render fields deeper than level 2 with 'null' values
    // In this case, only The Trunk.Item3 (Tree list) is after level 2
    let expected = """I like ("Leaf": 12,345) and ("Leaf": 12,345) and ("Trunk": [12,345, 20/05/2013 16:39:00 +09:30, [("Leaf": 12,345), ("Leaf": 12,345)]]) and ("ChairItem": ChairRecord { Back: "slanted", Legs: [1, 2, 3, 4, 5] })""" 
    
    MtAssert.RenderedAs( template, values, expected, provider, maxDepth=2)
    

[<Tests>]
let``Destructred F# objects captured with a custom destructurer render with format provider`` =
    let provider = CultureInfo("fr-FR")
    let template = "I like {@item1}
and {@item2}
and {@item3}
and {@item4}"
    let values : obj[] = [| Leaf 12.345
                            Leaf 12.345
                            Trunk (12.345, DateTimeOffset(2013, 5, 20, 16, 39, 00, TimeSpan.FromHours 9.5), [Leaf 12.345; Leaf 12.345])
                            Trunk (1.1, DateTimeOffset(2013, 5, 20, 16, 39, 00, TimeSpan.FromHours 9.5), [Seq [1.1;2.2;3.3]; Seq [4.4]])
                         |]
    let expected = """I like ("Leaf": 12,345)
and ("Leaf": 12,345)
and ("Trunk": [12,345, 20/05/2013 16:39:00 +09:30, [("Leaf": 12,345), ("Leaf": 12,345)]])
and ("Trunk": [1,1, 20/05/2013 16:39:00 +09:30, [("Seq": [1,1, 2,2, 3,3]), ("Seq": [4,4])]])"""
    MtAssert.RenderedAs( template, values, expected, provider)


type Size = Regular = 1 | Large = 2

type BernieSandersSizeFormatter (innerFormatProvider : IFormatProvider) =
  interface IFormatProvider with
    member this.GetFormat ty =
      match ty with
      | t when t = typeof<ICustomFormatter> -> this :> obj
      | _ -> innerFormatProvider.GetFormat ty
  interface ICustomFormatter with
    member this.Format (format : string, arg : obj, provider : IFormatProvider) =
      match arg with
      | :? Size as s ->
        match s with Size.Large -> "YUUUGE" | _ -> sprintf "%A" s
      | :? IFormattable as f ->
        f.ToString(format, innerFormatProvider)
      | _ ->
        arg.ToString()


[<Tests>]
let``Applies custom formatter to enums`` =
    let provider = (BernieSandersSizeFormatter CultureInfo.InvariantCulture) :> IFormatProvider
    let template = "Size {large} {regular}"
    let values : obj[] = [| Size.Large; Size.Regular |]
    let expected = "Size \"YUUUGE\" \"Regular\""
    MtAssert.RenderedAs( template, values, expected, provider)