open Chamonix

let parse_in_channel ic =
  let lexbuf = Lexing.from_channel ic in
  let lexer lexbuf =
    let token = Lexer.lexer lexbuf in
    Fmt.pr ">>> %a.\n%!" Parser.pp_token token ;
    token in
  try Ok (Parser.program lexer lexbuf)
  with
  | Parser.Error ->
    let a = Lexing.lexeme_start_p lexbuf in
    let b = Lexing.lexeme_end_p lexbuf in
    let location = Location.make a b in
    Error (`Msg (Fmt.str "Invalid snowball file (%a)" Location.pp location))
  | Lexer.Lexical_error chr ->
    let a = Lexing.lexeme_start_p lexbuf in
    let b = Lexing.lexeme_end_p lexbuf in
    let location = Location.make a b in
    Error (`Msg (Fmt.str "Invalid snowball file (%a, invalid character %02x)" Location.pp location
                   (Char.code chr)))

let parse_stdin () = parse_in_channel stdin

let () =
  match parse_stdin () with
  | Ok prgms -> Fmt.pr "%a" Fmt.(list ~sep:(any "\n") Program.pp) prgms
  | Error (`Msg err) -> Fmt.epr "%s.\n%!" err
