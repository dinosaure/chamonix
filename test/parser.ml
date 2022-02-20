let parse_in_channel ic =
  let open Chamonix in
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

let parse filename =
  Alcotest.test_case filename `Quick @@ fun () ->
  let ic = open_in ("../algorithms/" ^ filename) in
  let rs = parse_in_channel ic in
  close_in ic ; match rs with
  | Ok _ -> Alcotest.(check pass) "correct" () ()
  | Error (`Msg err) -> Alcotest.failf "%s." err

let algorithms =
  [ "arabic.sbl"
  ; "armenian.sbl"
  ; "basque.sbl"
  ; "catalan.sbl"
  ; "danish.sbl"
  ; "dutch.sbl"
  ; "english.sbl"
  ; "finnish.sbl"
  ; "french.sbl"
  ; "german2.sbl"
  ; "german.sbl"
  ; "greek.sbl"
  ; "hindi.sbl"
  ; "hungarian.sbl"
  ; "indonesian.sbl"
  ; "irish.sbl"
  ; "italian.sbl"
  ; "kraaij_pohlmann.sbl"
  ; "lithuanian.sbl"
  ; "lovins.sbl"
  ; "nepali.sbl"
  ; "norwegian.sbl"
  ; "porter.sbl"
  ; "portuguese.sbl"
  ; "romanian.sbl"
  ; "russian.sbl"
  ; "serbian.sbl"
  ; "spanish.sbl"
  ; "swedish.sbl"
  ; "tamil.sbl"
  ; "turkish.sbl"
  ; "yiddish.sbl" ]

let () = Alcotest.run "chamonix"
  [ "parser", List.map parse algorithms ]
