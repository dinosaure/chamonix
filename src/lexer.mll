{
open Parser

exception Lexical_error of char
exception Invalid_literal_string
exception Unterminated_comment
exception Invalid_string_escapes

let incr_line lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }
;;

let string_escape = ref ('{', '}')

}

let alpha = [ 'a'-'z' 'A'-'Z' ]
let digit = [ '0'-'9' ]
let print = [ '!'-'~' ]
let wsp = [ ' ' '\t' ]

let name = alpha (alpha | digit | '_') *

rule lexer = parse
  | eof             { EOF }
  | ' ' | '\t'      { lexer lexbuf }
  | '\''            { literal_string (Buffer.create 0x10) lexbuf }
  | '\n'            { incr_line lexbuf 0 ; lexer lexbuf }
  | "$"             { DOLLAR }
  | "("             { BRA }
  | ")"             { KET }
  | "*"             { MULTIPLY }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "/"             { DIVIDE }
  | "<"             { LS }
  | "="             { ASSIGN }
  | ">"             { GR }
  | "?"             { DEBUG }
  | "["             { LEFT_SLICE }
  | "]"             { RIGHT_SLICE }
  | "!="            { NE }
  | "*="            { MULTIPLY_ASSIGN }
  | "+="            { PLUS_ASSIGN }
  | "-="            { MINUS_ASSIGN }
  | "->"            { SLICE_TO }
  | "/="            { DIVIDE_ASSIGN }
  | "<+"            { INSERT }
  | "<-"            { SLICE_FROM }
  | "<="            { LE }
  | "=="            { EQ }
  | "=>"            { ASSIGN_TO }
  | ">="            { GE }
  | "as"            { AS }
  | "do"            { DO }
  | "or"            { OR }
  | "and"           { AND }
  | "for"           { FOR }
  | "get"           { GET }
  | "hex"           { HEX }
  | "hop"           { HOP }
  | "len"           { LEN }
  | "non"           { NON }
  | "not"           { NOT }
  | "set"           { SET }
  | "try"           { TRY }
  | "fail"          { FAIL }
  | "goto"          { GOTO }
  | "loop"          { LOOP }
  | "next"          { NEXT }
  | "size"          { SIZE }
  | "test"          { TEST }
  | "true"          { TRUE }
  | "among"         { AMONG }
  | "false"         { FALSE }
  | "lenof"         { LENOF }
  | "limit"         { LIMIT }
  | "unset"         { UNSET }
  | "atmark"        { ATMARK }
  | "attach"        { ATTACH }
  | "cursor"        { CURSOR }
  | "define"        { DEFINE }
  | "delete"        { DELETE }
  | "gopast"        { GOPAST }
  | "insert"        { INSERT }
  | "maxint"        { MAXINT }
  | "minint"        { MININT }
  | "repeat"        { REPEAT }
  | "sizeof"        { SIZEOF }
  | "tomark"        { TOMARK }
  | "atleast"       { ATLEAST }
  | "atlimit"       { ATLIMIT }
  | "decimal"       { DECIMAL }
  | "reverse"       { REVERSE }
  | "setmark"       { SETMARK }
  | "strings"       { STRINGS }
  | "tolimit"       { TOLIMIT }
  | "booleans"      { BOOLEANS }
  | "integers"      { INTEGERS }
  | "routines"      { ROUTINES }
  | "setlimit"      { SETLIMIT }
  | "backwards"     { BACKWARDS }
  | "externals"     { EXTERNALS }
  | "groupings"     { GROUPINGS }
  | "stringdef"     { STRINGDEF }
  | "substring"     { SUBSTRING }
  | "backwardmode"  { BACKWARDMODE }
  | "stringescapes" { string_escapes lexbuf ; lexer lexbuf }
  | "/*"            { block_of_comments lexbuf
                    ; lexer lexbuf }
  | "//"            { comment lexbuf
                    ; lexer lexbuf }
  | name as v       { NAME v }
  | digit+ as v     { NUMBER (int_of_string v) }
  | print+ as v     { IDENTIFIER v }
  | _ as chr        { raise (Lexical_error chr) }
and literal_string buf = parse
  | "'"             { LITERAL_STRING (!string_escape, Buffer.contents buf) }
  | "\n" as chr     { Buffer.add_char buf chr
                    ; incr_line lexbuf 0
                    ; literal_string buf lexbuf }
  | _ as chr        { Buffer.add_char buf chr
                    ; literal_string buf lexbuf }
  | eof             { raise Invalid_literal_string }
and comment = parse
  | "\n"            { incr_line lexbuf 0 ; () }
  | _               { comment lexbuf }
and block_of_comments = parse
  | "*/"            { () }
  | "\n"            { incr_line lexbuf 0 ; block_of_comments lexbuf }
  | eof             { raise Unterminated_comment }
  | _               { block_of_comments lexbuf }
and string_escapes = parse
  | wsp* (print as a) wsp* (print as b) wsp* '\n'
    { string_escape := (a, b)
    ; incr_line lexbuf 0
    ; () }
  | _ { raise Invalid_string_escapes }
