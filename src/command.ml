type t =
  | Or of t * t
  | And of t * t
  | Not of t
  | Try of t
  | Test of t
  | Fail of t
  | Do of t
  | Go_to of t
  | Go_past of t
  | Repeat of t
  | Set_mark of string
  | At_mark of Arithmetic.t
  | Loop of Arithmetic.t * t
  | At_least of Arithmetic.t * t
  | Hop of Arithmetic.t
  | Non of [ `Minus ] option * string
  | Replace_slice of [ `Literal_string of Literal_string.t | `Name of string ]
  | Move_slice of [ `Literal_string of Literal_string.t | `Name of string ]
  | At_limit
  | To_limit
  | Set of string
  | Unset of string
  | Next
  | Delete
  | Left_end
  | Right_end
  | True
  | False
  | Assign_to of string
  | Commands of t list
  | Routine of string
  | Among of [ `Search of Literal_string.t * string option
             | `Command of t list ] list
  | Backwards of t
  | String of string * t
  | Arithmetic_test of Test.t
  | Assign of string * Arithmetic.t
  | S of [ `Literal_string of Literal_string.t | `Name of string ]
  | Insert of [ `Literal_string of Literal_string.t | `Name of string ]
  | Set_limit of t * t
  | To_mark of Arithmetic.t
  | Substring

let rec pp ppf = function
  | Or (a, b) -> Fmt.pf ppf "%a or %a" pp a pp b
  | And (a, b) -> Fmt.pf ppf "%a and %a" pp a pp b
  | Not t -> Fmt.pf ppf "not %a" pp t
  | Try t -> Fmt.pf ppf "try %a" pp t
  | Test t -> Fmt.pf ppf "test %a" pp t
  | Fail t -> Fmt.pf ppf "fail %a" pp t
  | Do t -> Fmt.pf ppf "do %a" pp t
  | Go_to t -> Fmt.pf ppf "goto %a" pp t
  | Go_past t -> Fmt.pf ppf "gopast %a" pp t
  | Repeat t -> Fmt.pf ppf "repeat %a" pp t
  | Loop (a, t) -> Fmt.pf ppf "loop %a %a" Arithmetic.pp a pp t
  | At_least (a, t) -> Fmt.pf ppf "atleast %a %a" Arithmetic.pp a pp t
  | Hop a -> Fmt.pf ppf "hop %a" Arithmetic.pp a
  | Next -> Fmt.string ppf "next"
  | Delete -> Fmt.string ppf "delete"
  | Left_end -> Fmt.string ppf "["
  | Right_end -> Fmt.string ppf "]"
  | Set_mark name -> Fmt.pf ppf "setmark %s" name
  | Commands lst -> Fmt.pf ppf "(@[<1>%a@])" Fmt.(list ~sep:(any "@ ") pp) lst
  | Among lst ->
    let pp_line ppf = function
      | `Search (literal_string, Some name) ->
        Fmt.pf ppf "%a %s" Literal_string.pp literal_string name
      | `Search (literal_string, None) ->
        Literal_string.pp ppf literal_string
      | `Command cs ->
        Fmt.pf ppf "(%a)" Fmt.(list ~sep:(any "@ ") pp) cs in
    Fmt.pf ppf "among(@[<1>%a@])"
      Fmt.(list ~sep:(any "@\n") pp_line) lst
  | Routine name -> Fmt.string ppf name
  | Replace_slice (`Literal_string v) ->
    Fmt.pf ppf "<- %a" Literal_string.pp v
  | Replace_slice (`Name v) ->
    Fmt.pf ppf "<- %s" v
  | True -> Fmt.string ppf "true"
  | False -> Fmt.string ppf "false"
  | Backwards t -> Fmt.pf ppf "backwards %a" pp t
  | String (name, t) -> Fmt.pf ppf "$%s %a" name pp t
  | Arithmetic_test test -> Test.pp ppf test
  | Set name -> Fmt.pf ppf "set %s" name
  | Unset name -> Fmt.pf ppf "unset %s" name
  | S (`Literal_string v) -> Literal_string.pp ppf v
  | S (`Name v) -> Fmt.string ppf v
  | Assign (name, v) ->
    Fmt.pf ppf "%s = %a" name Arithmetic.pp v
  | Non (Some `Minus, name) -> Fmt.pf ppf "non -%s" name
  | Non (None, name) -> Fmt.pf ppf "non %s" name
  | Set_limit (a, b) -> Fmt.pf ppf "setlimit %a for %a" pp a pp b
  | To_mark v -> Fmt.pf ppf "tomark %a" Arithmetic.pp v
  | Substring -> Fmt.string ppf "substring"
  | Assign_to v -> Fmt.pf ppf "=> %s" v
  | Move_slice (`Literal_string v) -> Fmt.pf ppf "-> %a" Literal_string.pp v
  | Move_slice (`Name v) -> Fmt.pf ppf "-> %s" v
  | At_limit -> Fmt.string ppf "atlimit"
  | To_limit -> Fmt.string ppf "tolimit"
  | Insert (`Literal_string v) -> Fmt.pf ppf "insert %a" Literal_string.pp v
  | Insert (`Name v) -> Fmt.pf ppf "insert %s" v
  | At_mark v -> Fmt.pf ppf "atmark %a" Arithmetic.pp v
