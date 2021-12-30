type t =
  | Literal_string of Literal_string.t
  | Plus of t * t
  | Minus of t * t
