type t =
  | Minint | Maxint
  | Cursor | Limit
  | Size
  | Size_of of [ `Name of string | `Literal_string of Literal_string.t ]
  | Len
  | Len_of of [ `Name of string | `Literal_string of Literal_string.t ]
  | Plus of t * t
  | Minus of t * t
  | Multiply of t * t
  | Divide of t * t
  | Number of int
  | Name of string

let rec pp ppf = function
  | Minint -> Fmt.string ppf "minint"
  | Maxint -> Fmt.string ppf "maxint"
  | Cursor -> Fmt.string ppf "cursor"
  | Limit -> Fmt.string ppf "limit"
  | Size -> Fmt.string ppf "size"
  | Size_of (`Name v) -> Fmt.pf ppf "sizeof %s" v
  | Size_of (`Literal_string v) -> Fmt.pf ppf "sizeof %a" Literal_string.pp v
  | Len -> Fmt.string ppf "len"
  | Len_of (`Name v) -> Fmt.pf ppf "lenof %s" v
  | Len_of (`Literal_string v) -> Fmt.pf ppf "lenof %a" Literal_string.pp v
  | Plus (a, b) -> Fmt.pf ppf "%a + %a" pp a pp b
  | Minus (a, b) -> Fmt.pf ppf "%a - %a" pp a pp b
  | Multiply (a, b) -> Fmt.pf ppf "%a * %a" pp a pp b
  | Divide (a, b) -> Fmt.pf ppf "%a / %a" pp a pp b
  | Number v -> Fmt.int ppf v
  | Name v -> Fmt.string ppf v
