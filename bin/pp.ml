open Chamonix

let parse_in_channel ic =
  let lexbuf = Lexing.from_channel ic in
  try Ok (Parser.program Lexer.lexer lexbuf)
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
  | Ok prgms ->
    let gamma = Program.gamma prgms in
    Format.set_margin 80 ;
    Fmt.pr "%a" Fmt.(list ~sep:(any "\n") (Program.pp ~gamma)) prgms
  | Error (`Msg err) -> Fmt.epr "%s.\n%!" err
