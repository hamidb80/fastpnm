import std/[unittest, os, strformat]
import fastpnm


template checkSame(a, b): untyped =
  check a.width == b.width
  check a.height == b.height
  check a.maxValue == b.maxValue
  check a.data == b.data

proc exportPnm(size: int, m: PnmMagic): string =
  let
    fname = fmt"./temp/arrow-{size}-{m}.{fileExt(m)}"
    extra = case m
      of uncompressed: "-compress none"
      else: ""

  discard execShellCmd fmt"convert {extra} -flatten -background white -alpha remove -resize {size}x{size} ./examples/arrow.png {fname}"
  fname


suite "tests":
  discard existsOrCreateDir "./temp"

  for size in [7, 8, 9, 34, 37, 43, 120]:
    for (mraw, mbin) in [(P1, P4), (P2, P5), (P3, P6)]:
      test fmt"compare {mraw}:{mbin} at {size}x{size}":
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

