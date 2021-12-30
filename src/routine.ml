type t =
  | Or of t * t
  | And of t * t
  | Test of t
  | Repeat of t
  | Next
  | Slice_from of string * Literal_string.t
