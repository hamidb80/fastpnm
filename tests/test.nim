import std/[unittest, os, strformat]
import pbm, bitty

discard existsOrCreateDir "./temp"

proc exportPbm(size: int, m: PbmMagic): string =
  let
    fname = fmt"./temp/arrow-{size}-{m}.pbm"
    extra = case m
      of P1: "-compress none"
      of P4: ""

  discard execShellCmd fmt"convert {extra} -flatten -background white -alpha remove -resize {size}x{size} ./examples/arrow.png {fname}"
  fname


suite "test for different sizes":
  for size in [7, 8, 9, 34, 37, 43]:
    test "test for size: " & $size:
      let
        p1 = parsePbm readFile exportPbm(size, P1)
        p4 = parsePbm readFile exportPbm(size, P4)

      check p1.data == p4.data

suite "re-read":
  test "P1":
      let
          before = parsePbm readFile exportPbm(8, P1)
          aftere = parsePbm $before

      check before == aftere

  test "P4":
      let
          before = parsePbm readFile exportPbm(7, P4)
          aftere = parsePbm $before

      check before.data.len == aftere.data.len
      check before.data == aftere.data
