#I "bin/Debug/netstandard2.1/publish/"
#r "Hopac.Core"
#r "Hopac.Platform"
#r "Hopac"
#r "NodaTime"
#r "Ply"
#r "FParsec"
#r "Logary"

open System
open Logary
open Logary.Trace

let spanLogger =
  SpanBuilder("new_span")
    .setAttribute("test", true)
    .debug()
    .start()


let span = spanLogger.finish()


type [<Struct>] Width = Width of width: float
type [<Struct>] Height = Height of height: float
type [<Struct>] Viewport = Viewport of w: Width * h: Height
type [<Struct>] Coord = Coord of x: float * y: float

type CurveCMD =
  | Cubic of start: Coord * ending: Coord * c1: Coord * c2: Coord
  | Quadratic of start: Coord * ending: Coord * c1: Coord
type [<Struct>] LineCMD =
  | L of point: Coord
  | H of x: float
  | V of y: float
/// https://www.w3.org/TR/SVG2/paths.html
type PathCMD =
  /// M (absolute), m (relative)
  | MoveTo of point: Coord * relative: bool
  /// L (absolute), l (relative)
  | LineTo of LineCMD * relative: bool
  /// Cubic: C, c, S, s; Quadratic: Q, q, T, t; Elliptical: A, a
  | CurveTo of curve: CurveCMD * relative: bool
  /// Z: connect back to the 'initial point'
  | ClosePath
  member x.isSegment = match x with MoveTo _ | ClosePath -> false | _ -> true

type Contents =
  | Text of text: string
  | Path of d: PathCMD[]
type Layer = Layer of topLeft: Coord * bottomRight: Coord * contents: Contents
type Composed = Composed of layers: Layer[]
type [<Struct>] RGBA = RGBA of r: single * g: single * b:single * a: single
module RGBA =
  let Black = RGBA (0.f, 0.f, 0.f, 1.f)
  let White = RGBA (1.f, 1.f, 1.f, 1.f)
  let Red = RGBA (1.f, 0.f, 0.f, 1.f)
  let Green = RGBA (0.f, 1.f, 0.f, 1.f)
  let Blue = RGBA (0.f, 0.f, 1.f, 1.f)
  let Transparent = RGBA (1.f, 1.f, 1.f, 0.f)
type RGBA with
  static member Zero = RGBA.Transparent
type [<Struct>] Canvas =
  Canvas of w: Width * h: Height * d: RGBA[,]
with
  member self.width = let (Canvas (w, _, _)) = self in w
  member self.height = let (Canvas (_, h, _)) = self in h
  member self.Item
    with get(x: int, y: int) = let (Canvas (_, _, d)) = self in d.[x, y]
     and set(x: int, y: int) value = let (Canvas (_, _, d)) = self in d.[x, y] <- value
  static member create(Width w, Height h) =
    let wi, hi = int (Math.Round(w)), int (Math.Round(h))
    Canvas (Width (float wi), Height (float hi), Array2D.zeroCreate<RGBA> wi hi)

// drawing
let draw (Canvas (w, h, data)) (Composed layers) =
  // start at bottom layer and draw on top of it
  for i = layers.GetLength(0) - 1 downto 0 do
    let (Layer (Coord (layerTLx, layerTLy), _, contents)) = layers.[i]
    let mutable cursor = struct (layerTLx, layerTLy)
    match contents with
    | Text text ->
      ()
    | Path pathCMDS ->
      for p = 0 to pathCMDS.Length do
        match pathCMDS.[p], cursor with
        | MoveTo (Coord (dx, dy), relative), struct (cx, cy) when relative ->
          cursor <- struct (cx + dx, cy + dy)
        | MoveTo (Coord (x, y), relative), struct (cx, cy) ->
          cursor <- struct (x, y)
        | _ ->
          ()

    printfn "%A" data

// TODO: ASCII art from Canvas
type [<Struct>] CharCanvas =
  CharCanvas of w: uint16 * h: uint16 * d: char[,]
with
  member self.width = let (CharCanvas (w, _, _)) = self in w
  member self.height = let (CharCanvas (_, h, _)) = self in h
  member self.Item
    with get(x: int, y: int) = let (CharCanvas (_, _, d)) = self in d.[x, y]
     and set(x: int, y: int) value = let (CharCanvas (_, _, d)) = self in d.[x, y] <- value
  static member create(w, h) =
    CharCanvas (w, h, Array2D.zeroCreate<char> (int w) (int h))

module CharCanvas =
  let ofCanvas (Canvas (Width w, Height h, _)): CharCanvas =
    let w, h = uint16 (Math.Round(w)), uint16 (Math.Round(h))
    // TODO: https://bitesofcode.wordpress.com/2017/01/19/converting-images-to-ascii-art-part-1/
    CharCanvas.create(w, h)
type CharCanvas with
  static member ofCanvas c = CharCanvas.ofCanvas c

// TODO: print SVG from Layer + Contents
type SVG =
  | PathETC