import std/[unittest, os, strformat]
import pan

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
  # for magic in P1..P6:
  for magic in P1..P1:
    # for size in [7, 8, 9, 34, 37, 43, 120]:
    for size in [7]:
      # test fmt"compare-{size}":
      #   let
      #     p1 = parsePan readFile exportPan(size, bitMapRaw)
      #     p4 = parsePan readFile exportPan(size, bitMapBinray)
      
      test fmt"re read {magic} {size}":
        let
          before = parsePan readFile exportPan(size, magic)
          aftere = parsePan $before

        echo before

  # test "P4":
  #     let
  #         before = parsePan readFile exportPan(7, P4)
  #         aftere = parsePan $before

  #     check before.b2.len == aftere.b2.len
  #     check before.b2 == aftere.b2


  let pgm = parsePan readFile "./temp/aaaaaaaa.pgm"
