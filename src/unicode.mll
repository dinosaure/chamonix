{
let digit = function
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | _ -> assert false

let num lexbuf ~base ~first ~last =
  let c = ref 0 in
  for i = first to last do
    let v = digit (Lexing.lexeme_char lexbuf i) in
    c := (base * !c) + v
  done ; !c

let uchar lexbuf =
  let len = Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf in
  let first = 2 (* skip opening U+ *) in
  let last = len - 1 in
  let digit_count = last - first + 1 in
  match digit_count > 6 with
  | true -> failwith "too many digits, expected 1 to 6 hexadecimal digits"
  | false ->
    let cp = num lexbuf ~base:16 ~first ~last in
    if Uchar.is_valid cp then Uchar.unsafe_of_int cp
    else Fmt.failwith "%X is not a Unicode scalar value" cp

}

let hex = [ '0'-'9' 'a'-'f' 'A'-'F' ]

rule unicode = parse
  | "U+" hex+ { uchar lexbuf }
