type 'v t =
  | Minint | Maxint
  | Cursor | Limit
  | Size
  | Size_of of [ `Name of 'v | `Literal_string of Literal_string.t ]
  | Len
  | Len_of of [ `Name of 'v | `Literal_string of Literal_string.t ]
  | Plus of 'v t * 'v t
  | Minus of 'v t * 'v t
  | Multiply of 'v t * 'v t
  | Divide of 'v t * 'v t
  | Number of int
  | Name of 'v
(* XXX(dinosaure): [Name]/[`Name] is a variable. *)

type de_bruijn_index = int t
type bound_variables = string t

type 'e t' =
  | Minint : 'e t'
  | Maxint : 'e t'
  | Cursor : 'e t'
  | Limit : 'e t'
  | Val : int -> 'e t'
  | Len : [ `Byte | `UTF_8 ] -> 'e t'
  | Len_of : [ `Byte | `UTF_8 ] * ('e, string) Var.t -> 'e t'
  | Var : ('e, int) Var.t -> 'e t'
  | Bin : [ `Add | `Sub | `Mul | `Div ] * 'e t' * 'e t' -> 'e t'

let utf_8_string_length str =
  let folder acc _idx = function
    | `Malformed str -> acc + String.length str
    | `Uchar _ -> succ acc in
  Uutf.String.fold_utf_8 folder 0 str

let safe () = assert false

let rec eval
  : type e. gamma:Ty.value list -> state:State.t -> e t' -> int
  = fun ~gamma ~state -> function
  | Minint -> State.minint state
  | Maxint -> State.maxint state
  | Cursor -> State.cursor state
  | Limit  -> State.limit  state
  | Val v -> v
  | Len t -> State.len t state
  | Len_of (`Byte, var) ->
    ( match List.nth gamma (Var.to_int var) with
    | Ty.Value (String, str) -> String.length str
    | _ -> safe () )
  | Len_of (`UTF_8, var) ->
    ( match List.nth gamma (Var.to_int var) with
    | Ty.Value (String, str) -> utf_8_string_length str
    | _ -> safe () )
  | Var var ->
    ( match List.nth gamma (Var.to_int var) with
    | Ty.Value (Int, n) -> n
    | _ -> safe () )
  | Bin (`Add, a, b) -> eval ~gamma ~state a + eval ~gamma ~state b
  | Bin (`Sub, a, b) -> eval ~gamma ~state a - eval ~gamma ~state b
  | Bin (`Mul, a, b) -> eval ~gamma ~state a * eval ~gamma ~state b
  | Bin (`Div, a, b) -> eval ~gamma ~state a / eval ~gamma ~state b

type expr = Expr : 'e t' * 'e Gamma.t -> expr

exception Unbound_variable of string

let rec index ~gamma : bound_variables -> de_bruijn_index = function
  | Minint -> Minint | Maxint -> Maxint
  | Cursor -> Cursor | Limit -> Limit
  | Size -> Size
  | Len -> Len
  | Size_of (`Name name) ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Size_of (`Name index)
    | None -> raise (Unbound_variable name) )
  | Size_of (`Literal_string _ as v) -> Size_of v
  | Len_of (`Name name) ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Size_of (`Name index)
    | None -> raise (Unbound_variable name) )
  | Len_of (`Literal_string _ as v) -> Len_of v
  | Name name ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Name index
    | None -> raise (Unbound_variable name) )
  | Plus (a, b) -> Plus (index ~gamma a, index ~gamma b)
  | Minus (a, b) -> Minus (index ~gamma a, index ~gamma b)
  | Multiply (a, b) -> Multiply (index ~gamma a, index ~gamma b)
  | Divide (a, b) -> Divide (index ~gamma a, index ~gamma b)
  | Number n -> Number n

let typ ~constants ~gamma expr =
  let rec go : gamma:Gamma.gamma -> de_bruijn_index -> (expr, de_bruijn_index Error.v) result = fun ~gamma expr ->
    let Gamma.V gamma' = Gamma.typ gamma in
    match expr with
    | Size_of (`Literal_string v) ->
      let str = Literal_string.to_utf_8_string ~gamma:constants v in
      Ok (Expr (Val (String.length str), gamma'))
    | Len_of (`Literal_string v) ->
      let str = Literal_string.to_utf_8_string ~gamma:constants v in
      Ok (Expr (Val (utf_8_string_length str), gamma'))
    | Len -> Ok (Expr (Len `UTF_8, gamma'))
    | Size -> Ok (Expr (Len `Byte, gamma'))
    | (Size_of (`Name name) | Len_of (`Name name) as len_of) ->
      let count = match len_of with
        | Size_of _ -> `Byte | Len_of _ -> `UTF_8
        | _ -> safe () in
      ( try ( match Var.typ ~gamma name with
            | Var.V (x', g'', Ty.String) -> Ok (Expr (Len_of (count, x'), g''))
            | Var.V (_, _, t') ->
              Error (Error.v expr gamma [ Error.Type_mismatch { a= Ty.V Ty.String; b= Ty.V t' } ]) )
        with
        | Var.Unbound_variable _ ->
          Error (Error.v expr gamma [ Error.Unbound_variable ]) )
    | Number n -> Ok (Expr (Val n, gamma'))
    | Name name ->
      ( try ( match Var.typ ~gamma name with
            | Var.V (x', g'', Ty.Int) -> Ok (Expr (Var x', g''))
            | Var.V (_, _, t') ->
              Error (Error.v expr gamma [ Error.Type_mismatch { a= Ty.V Ty.Int; b= Ty.V t' } ]) )
        with
        | Var.Unbound_variable _ ->
          Error (Error.v expr gamma [ Error.Unbound_variable ]) )
    | Minint -> Ok (Expr (Minint, gamma'))
    | Maxint -> Ok (Expr (Maxint, gamma'))
    | Cursor -> Ok (Expr (Cursor, gamma'))
    | Limit -> Ok (Expr (Limit, gamma'))
    | (Plus (a, b) | Minus (a, b) | Multiply (a, b) | Divide (a, b) as operation) ->
      let operator = match operation with
        | Plus _ -> `Add | Minus _ -> `Sub | Multiply _ -> `Mul | Divide _ -> `Div
        | _ -> assert false (* XXX(dinosaure): safe. *) in
      ( match go ~gamma a, go ~gamma b with
      | Ok (Expr (a', g0)),
        Ok (Expr (b', g1)) ->
        ( match Gamma.equal g0 g1 with
        | Some Refl.Refl -> Ok (Expr (Bin (operator, a', b'), g0))
        | None ->
          Error (Error.v expr gamma [ Error.Gamma_mismatch { g0= Gamma.V g0; g1= Gamma.V g1; } ]) )
      | Error err, _ | _, Error err -> Error err ) in
  go ~gamma expr

let rec pp ~gamma : bound_variables Fmt.t = fun ppf -> function
  | Minint -> Fmt.string ppf "minint"
  | Maxint -> Fmt.string ppf "maxint"
  | Cursor -> Fmt.string ppf "cursor"
  | Limit -> Fmt.string ppf "limit"
  | Size -> Fmt.string ppf "size"
  | Size_of (`Name v) -> Fmt.pf ppf "sizeof %s" v
  | Size_of (`Literal_string v) -> Fmt.pf ppf "sizeof %a" (Literal_string.pp ~gamma) v
  | Len -> Fmt.string ppf "len"
  | Len_of (`Name v) -> Fmt.pf ppf "lenof %s" v
  | Len_of (`Literal_string v) -> Fmt.pf ppf "lenof %a" (Literal_string.pp ~gamma) v
  | Plus (a, b) -> Fmt.pf ppf "%a + %a" (pp ~gamma) a (pp ~gamma) b
  | Minus (a, b) -> Fmt.pf ppf "%a - %a" (pp ~gamma) a (pp ~gamma) b
  | Multiply (a, b) -> Fmt.pf ppf "%a * %a" (pp ~gamma) a (pp ~gamma) b
  | Divide (a, b) -> Fmt.pf ppf "%a / %a" (pp ~gamma) a (pp ~gamma) b
  | Number v -> Fmt.int ppf v
  | Name v -> Fmt.string ppf v
