
import "npeg"
import "strutils"

type
  TokenKind = enum
    tkIndent,
    tkLet,
    tkIf,
    tkElse,
    tkLitTrue,
    tkLitFalse,
    tkLitInt,
    tkLitString,
    tkEquals,
    tkAssign,
    tkSymbol,
    tkColon,


  Token = object
    case kind: TokenKind
      of tkIndent:
        indent: int
      of tkLitInt:
        litInt: int
      of tkLitString:
        litString: string
      of tkSymbol:
        symbol: string
      else:
        discard

  TokenList = seq[Token]


# lexer

let p = peg("things", ts: TokenList):

    # Helpers

    newline <- "\n" | "\r\n"
    S <- *" "
    ident <- (Alpha | '_') * *(Alpha | Digit | '_')

    # Lexer
    
    things <- *thing * !1
    thing <- indent * *(token * S) * newline
    indent <- *" ": ts.add Token(kind: tkIndent, indent: len($0))

    token <- keyword | binop | symbol | lit | colon

    colon <- ":": ts.add Token(kind: tkColon)

    keyword <- kw_let | kw_if | kw_else
    kw_let <- "let" * S: ts.add Token(kind: tkLet)
    kw_if <- "if" * S: ts.add Token(kind: tkIf)
    kw_else <- "else" * S: ts.add Token(kind: tkElse)

    binop <- binop_equals | binop_assign
    binop_equals <- "==": ts.add Token(kind: tkEquals)
    binop_assign <- "=": ts.add Token(kind: tkAssign)

    symbol <- >ident: ts.add Token(kind: tkSymbol, symbol: $1)

    lit <- lit_true | lit_false | lit_number | lit_string
    lit_true <- "true": ts.add Token(kind: tkLitTrue)
    lit_false <- "false": ts.add Token(kind: tkLitFalse)
    lit_number <- +Digit: ts.add Token(kind: tkLitInt, litInt: parseInt($0))
    lit_string <- '"' * *(1-'"') * '"': ts.add Token(kind: tkLitString, litString: $0)


proc `$`(t: Token): string =
  result.add ($t.kind)[2..^1]
  result.add " "
  case t.kind:
    of tkLitInt: result.add($t.litInt)
    of tkLitString: result.add($t.litString)
    of tkIndent: result.add($t.indent)
    of tkSymbol: result.add($t.symbol)
    else: discard
  result.add "\n"


proc `$`(ts: TokenList): string =
  for t in ts:
    result.add($t)

var ts: TokenList
echo p.matchFile("hello.nim", ts).ok
echo $ts


# vi: set ft=nim et
