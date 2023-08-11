import std/[unittest, os, strformat, sequtils, strutils]
import fastpnm


template checkSame(a, b): untyped =
  check a.width == b.width
  check a.height == b.height
  check a.maxValue == b.maxValue
  check a.comment == b.comment
  assert a.data == b.data

proc exportPnm(size: int, m: PnmMagic): string =
  let
    fname = fmt"./temp/arrow-{size}-{m}.{fileExt(m)}"
    extra = case m
      of uncompressed: "-compress none"
      else: ""

  discard execShellCmd fmt"convert {extra} -flatten -background white -alpha remove -resize {size}x{size} ./examples/arrow.png {fname}"
  fname

func c(r, g, b: uint8): Color = (r, g, b)
func `$`(c: Color): string =
  c.r.toHex & c.g.toHex & c.b.toHex


suite "raw & binary":
  discard existsOrCreateDir "./temp"

  for size in [7, 8, 9, 19, 34, 37, 43, 120]:
    for (mraw, mbin) in [(P1, P4), (P2, P5), (P3, P6)]:
      test fmt"compare {mraw} with {mbin} at {size}x{size}":
        let
          r = parsePnm readFile exportPnm(size, mraw)
          b = parsePnm readFile exportPnm(size, mbin)
        checkSame r, b

    for magic in P1..P6:
      test fmt"re read {magic} at {size}x{size}":
        let
          prev = parsePnm readFile exportPnm(size, magic)
          next = parsePnm $prev
        checkSame prev, next

suite "correctness":
  test "P1":
    let pbm = parsePnm readFile "./examples/j.pbm"
    var ones: seq[Position]

    for y in 0..<pbm.height:
      for x in 0..<pbm.width:
        if pbm.getBool(x, y):
          ones.add (x, y)

    check ones == @[
      (4, 0),
      (4, 1),
      (4, 2),
      (4, 3),
      (4, 4),
      (4, 5),
      (0, 6),
      (4, 6),
      (1, 7),
      (2, 7),
      (3, 7)]

  test "P2":
    let
      pgm = parsePnm readFile "./examples/4x6.pgm"
      numbers = cast[seq[uint8]](pgm.data)
    check numbers == toseq(0'u8 .. 23'u8)
    check pgm.getGrayScale(1, 5) == 21
    check pgm.getGrayScale(3, 1) == 7

  test "P3":
    let ppm = parsePnm readFile "./examples/colorful.ppm"
    check get2d[Color](ppm) == @[
      @[c(255, 0, 0), c(0, 255, 0), c(0, 0, 255)],
      @[c(255, 255, 0), c(255, 255, 255), c(0, 0, 0)]]

suite "special cases":
  test "P1 no space":
    let
      j1 = parsePnm readfile "./examples/j.pbm"
      j2 = parsePnm readfile "./examples/j_no_space.pbm"

    check j1.data == j2.data

  test "P3 variant spaces":
    let
      img1 = parsePnm readfile "./examples/p3_lines.ppm"
      img2 = parsePnm readfile "./examples/p3_spaces.ppm"

    checkSame img1, img2

suite "2D":
  const
    T = true
    F = false

  test "from2d":
    let p = from2d @[
      @[T, T, F, T, F, F, T, T, F, T],
      @[F, F, T, F, T, T, F, F, T, T],
      @[T, T, F, F, T, F, T, T, F, F]]

    check cast[seq[uint8]](p.data) == @[
      0b11010011'u8, 0b01000000'u8,
      0b00101100'u8, 0b11000000'u8,
      0b11001011'u8, 0b00000000'u8]

suite "comments":
  let p = parsePnm(readfile "./examples/j_no_space.pbm", true)

  test "captures comment":
    check p.comment == "This is an example bitmap of the letter \"J\""

  test "writes comment":
    let c = "Hello! my name\nis Hamid!"
    var ppp = p
    ppp.comment = c
    let again = parsePnm(`$`(ppp, addComments = true), true)
    checkSame ppp, again
