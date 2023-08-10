# pattern according to https://oceancolor.gsfc.nasa.gov/staff/norman/seawifs_image_cookbook/faux_shuttle/pbm.html
# magic :: P1/P4
# whitespace/comment
# width
# whitespace/comment
# height
# whitespace/comment
# content :: 0 1

import std/[strutils, parseutils, bitops]
import bitty

type
    PbmParserState = enum
        ppsMagic
        ppsWidth
        ppsHeight
        ppsContent

    PbmMagic* = enum
        P1 ## uncompressed :: each bit as a single char `1` or `0`
        P4 ## compressed :: stored i n byte like `10101011`

    Pbm* = object
        magic*: PbmMagic
        width*, height*: Natural
        comments*: seq[string]
        data*: BitArray

# ----- utils

template addMulti(z, a, b): untyped =
    z.add a
    z.add b

template addMulti(z, a, b, c): untyped =
    z.add a
    z.add b
    z.add c

func toDigit(b: bool): char =
    case b
    of true: '1'
    of false: '0'

func addByte(arr: var BitArray, b: byte, cutAfter: range[0..7]) =
    for i in countdown(7, cutAfter):
        arr.add testbit(b, i)

func ceilDiv(n, d: int): int =
    let t = n div d
    if n mod d == 0: t
    else: t + 1

func checkInRange(pbm: Pbm, x, y: int): bool =
    x in 0 ..< pbm.width and
    y in 0 ..< pbm.height

func getIndex(pbm: Pbm, x, y: int): int =
    y*pbm.width + x

# ----- API

func parsePbmContent*(s: string, offset: int, result: var Pbm) =
    let
        size = s.len - offset
        extraBits = result.width mod 8
        bytesRow = result.width.ceilDiv 8

        limit =
            if extraBits == 0: 0
            else: 8-extraBits


    for i in 0 ..< size:
        let ch = s[i + offset]
        case result.magic
        of P1:
            case ch
            of Whitespace: discard
            of '1': result.data.add true
            of '0': result.data.add false
            else: raise newException(ValueError,
                    "expected 1 or 0 in data section but got '" & ch & '\'')

        of P4:
            let
                cut =
                    if i mod bytesRow == bytesRow-1: limit
                    else: 0

            result.data.addByte cast[byte](ch), cut

func `[]`*(pbm: Pbm, x, y: int): bool =
    assert pbm.checkInRange(x, y)
    pbm.data[pbm.getIndex(x, y)]

func `[]=`*(pbm: Pbm, x, y: int, b: bool): bool =
    assert pbm.checkInRange(x, y)
    pbm.data[pbm.getIndex(x, y)] = b

func parsePbm*(s: string, captureComments = false): Pbm =
    var
        lastCh = '\n'
        i = 0.Natural
        state = ppsMagic

    while i != s.len:
        let ch = s[i]

        if (lastCh in Newlines) and (ch == '#'):
            let newi = s.find('\n', i+1)
            if captureComments:
                result.comments.add s[i+1 ..< newi]
            i = newi
        elif ch in Whitespace: inc i
        else:
            case state
            of ppsMagic:
                var word: string
                inc i, s.parseIdent(word, i)
                case word.toUpperAscii
                of "P1": result.magic = P1
                of "P4": result.magic = P4
                else: raise newException(ValueError, "invalid magic: '" & ch & '\'')
                inc state

            of ppsWidth:
                inc i, s.parseInt(result.width, i)
                inc state

            of ppsHeight:
                inc i, s.parseInt(result.height, i)
                inc state

            of ppsContent:
                result.data = newBitArray()
                parsePbmContent s, i, result
                break

        lastch = ch

func `$`*(pbm: Pbm, addComments = true): string =
    result.addMulti $pbm.magic, '\n'

    for c in pbm.comments:
        result.addMulti '#', $pbm.magic, '\n'

    result.addMulti $pbm.width, ' '
    result.addMulti $pbm.height, '\n'

    case pbm.magic
    of P1:
        for i, b in pbm.data:
            let whitespace =
                if i+1 == pbm.width: '\n'
                else: ' '

            result.addMulti toDigit(b), whitespace

    of P4:
        # let cd = pbm.data.len.ceilDiv 8
        # for i, b in pbm.data:
        #     result.add cast[char](b)
        raise newException(ValueError, "not implemented")
