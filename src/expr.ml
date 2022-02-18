type t =
  | Literal_string of Literal_string.t
  | Plus of t * t
  | Minus of t * t

let rec pp ppf = function
  | Literal_string v -> Literal_string.pp ppf v
  | Plus (a, b) -> Fmt.pf ppf "%a + %a" pp a pp b
  | Minus (a, b) -> Fmt.pf ppf "%a - %a" pp a pp b
