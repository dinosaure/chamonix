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

type 'v t =
  | Test of 'v Arithmetic.t * operator * 'v Arithmetic.t
  | Test_from_gamma of 'v * operator * 'v Arithmetic.t

type de_bruijn_index = int t
type bound_variables = string t

type ('e, 'a) t' =
  | Var : ('e, int ref) Var.t -> ('e, int) t'
  | Val : 'e Arithmetic.t' -> ('e, int) t'
  | Bin :
      ('e, 'a) t' * [ `Eq | `Neq | `Gt | `Gte | `Lt | `Lte ] * ('e, 'a) t'
      -> ('e, bool) t'
  | Non : ('e, bool) t' -> ('e, bool) t'

let safe () = assert false

let rec eval :
    type e a.
    gamma:Ty.value list ->
    mode:[ `Forward | `Backward ] ->
    state:State.t ->
    (e, a) t' ->
    a =
 fun ~gamma ~mode ~state -> function
  | Val a -> Arithmetic.eval ~gamma ~mode ~state a
  | Var var -> (
      match List.nth gamma (Var.to_int var) with
      | Ty.Value (Int, n) -> !n
      | _ -> safe ())
  | Non v -> not (eval ~gamma ~mode ~state v)
  | Bin (a, `Eq, b) -> eval ~gamma ~mode ~state a = eval ~gamma ~mode ~state b
  | Bin (a, `Neq, b) -> eval ~gamma ~mode ~state a <> eval ~gamma ~mode ~state b
  | Bin (a, `Gt, b) -> eval ~gamma ~mode ~state a > eval ~gamma ~mode ~state b
  | Bin (a, `Gte, b) -> eval ~gamma ~mode ~state a >= eval ~gamma ~mode ~state b
  | Bin (a, `Lt, b) -> eval ~gamma ~mode ~state a < eval ~gamma ~mode ~state b
  | Bin (a, `Lte, b) -> eval ~gamma ~mode ~state a <= eval ~gamma ~mode ~state b

type expr = Expr : ('e, bool) t' * 'e Gamma.t -> expr

exception Unbound_variable of string

let index ~gamma : bound_variables -> de_bruijn_index = function
  | Test (a, o, b) ->
      Test (Arithmetic.index ~gamma a, o, Arithmetic.index ~gamma b)
  | Test_from_gamma (name, o, a) -> (
      match Hashtbl.find_opt gamma name with
      | Some index -> Test_from_gamma (index, o, Arithmetic.index ~gamma a)
      | None -> raise (Unbound_variable name))

let typ ~constants ~gamma expr =
  let go :
      gamma:Gamma.gamma ->
      de_bruijn_index ->
      (expr, de_bruijn_index Error.v) result =
   fun ~gamma expr ->
    match expr with
    | Test (a, o, b) -> (
        let operator =
          match o with
          | Equal -> `Eq
          | Not_equal -> `Neq
          | Greater -> `Gt
          | Greater_equal -> `Gte
          | Less -> `Lt
          | Less_equal -> `Lte
        in
        match
          ( Arithmetic.typ ~constants ~gamma a,
            Arithmetic.typ ~constants ~gamma b )
        with
        | Ok (Arithmetic.Expr (a', g0)), Ok (Arithmetic.Expr (b', g1)) -> (
            match Gamma.equal g0 g1 with
            | Some Refl.Refl -> Ok (Expr (Bin (Val a', operator, Val b'), g0))
            | None ->
                Error
                  (Error.v expr gamma
                     [
                       Error.Gamma_mismatch { g0 = Gamma.V g0; g1 = Gamma.V g1 };
                     ]))
        | Error err, _ | _, Error err -> Error (Error.map_expr expr err))
    | Test_from_gamma (name, o, v) -> (
        let operator =
          match o with
          | Equal -> `Eq
          | Not_equal -> `Neq
          | Greater -> `Gt
          | Greater_equal -> `Gte
          | Less -> `Lt
          | Less_equal -> `Lte
        in
        match (Var.typ ~gamma name, Arithmetic.typ ~constants ~gamma v) with
        | Var.V (x', g0, Ty.Int), Ok (Arithmetic.Expr (v', g1)) -> (
            match Gamma.equal g0 g1 with
            | Some Refl.Refl -> Ok (Expr (Bin (Var x', operator, Val v'), g0))
            | None ->
                Error
                  (Error.v expr gamma
                     [
                       Error.Gamma_mismatch { g0 = Gamma.V g0; g1 = Gamma.V g1 };
                     ]))
        | _, Error err -> Error (Error.map_expr expr err)
        | Var.V (_, _, a), _ ->
            Error
              (Error.v expr gamma
                 [ Error.Type_mismatch { a = Ty.V a; b = Ty.V Ty.Int } ]))
  in
  go ~gamma expr

let pp ~gamma : bound_variables Fmt.t =
 fun ppf -> function
  | Test (a, op, b) ->
      Fmt.pf ppf "$(%a %a %a)" (Arithmetic.pp ~gamma) a pp_operator op
        (Arithmetic.pp ~gamma) b
  | Test_from_gamma (name, op, a) ->
      Fmt.pf ppf "$%s %a %a" name pp_operator op (Arithmetic.pp ~gamma) a
