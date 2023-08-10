import std/[unittest, os, strformat]
import fastpnm

discard existsOrCreateDir "./temp"

proc exportPan(size: int, m: PanMagic): string =
  let
    fname = fmt"./temp/arrow-{size}-{m}.{fileExt(m)}"
    extra = case m
      of P1, P2, P3: "-compress none"
      else: ""

  discard execShellCmd fmt"convert {extra} -flatten -background white -alpha remove -resize {size}x{size} ./examples/arrow.png {fname}"
  fname


suite "tests":
  for size in [7, 8, 9, 34, 37, 43, 120]:
    for (mraw, mbin) in [(P1, P4), (P2, P5), (P3, P6)]:
      test fmt"compare {mraw}:{mbin} at {size}x{size}":
        let
          r = parsePan readFile exportPan(size, mraw)
          b = parsePan readFile exportPan(size, mbin)
        # echo r
        check r.data == b.data

    for magic in P1..P6:
      test fmt"re read {magic} at {size}x{size}":
        let
          prev = parsePan readFile exportPan(size, magic)
          next = parsePan $prev
        check prev.data == next.data
