type operator =
  | Equal
  | Not_equal
  | Greater
  | Greater_equal
  | Less
  | Less_equal

let pp_operator ppf = function
  | Equal -> Fmt.string ppf "=="
  | Not_equal -> Fmt.string ppf "!="
  | Greater -> Fmt.string ppf ">"
  | Greater_equal -> Fmt.string ppf ">="
  | Less -> Fmt.string ppf "<"
  | Less_equal -> Fmt.string ppf "<="

type t =
  | Test of Arithmetic.t * operator * Arithmetic.t
  | Test_from_gamma of string * operator * Arithmetic.t

let pp ppf = function
  | Test (a, op, b) ->
    Fmt.pf ppf "$(%a %a %a)" Arithmetic.pp a pp_operator op Arithmetic.pp b
  | Test_from_gamma (name, op, a) ->
    Fmt.pf ppf "$%s %a %a" name pp_operator op Arithmetic.pp a
