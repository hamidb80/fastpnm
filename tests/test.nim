import std/[unittest, os, strformat]
import pan, bitty

discard existsOrCreateDir "./temp"

proc exportPan(size: int, m: PanMagic): string =
  let
    fname = fmt"./temp/arrow-{size}-{m}.pbm"
    extra = case m.code
      of 1: "-compress none"
      else: ""

  discard execShellCmd fmt"convert {extra} -flatten -background white -alpha remove -resize {size}x{size} ./examples/arrow.png {fname}"
  fname


suite "test for different sizes":
  for size in [7, 8, 9, 34, 37, 43]:
    test fmt"test for size-{size}":
      let
        p1 = parsePan readFile exportPan(size, bitMapRaw)
        p4 = parsePan readFile exportPan(size, bitMapBinray)

      check p1.b2 == p4.b2

suite "re-read":
  test "P1":
      let
          before = parsePan readFile exportPan(8, P1)
          aftere = parsePan $before

      check before.b2 == aftere.b2

  test "P4":
      let
          before = parsePan readFile exportPan(7, P4)
          aftere = parsePan $before

      check before.b2.len == aftere.b2.len
      check before.b2 == aftere.b2
