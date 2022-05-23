type 'v t =
  | Leaf of [ `Name of 'v | `Literal_string of Literal_string.t ]
  | Add of 'v t * 'v t
  | Sub of 'v t * 'v t

exception Unbound_variable of string

let rec index ~gamma = function
  | Leaf (`Literal_string v) -> Leaf (`Literal_string v)
  | Add (a, b) -> Add (index ~gamma a, index ~gamma b)
  | Sub (a, b) -> Sub (index ~gamma a, index ~gamma b)
  | Leaf (`Name name) -> (
      match Hashtbl.find_opt gamma name with
      | Some idx -> Leaf (`Name idx)
      | None -> raise (Unbound_variable name))

let pp_s ~gamma ppf = function
  | `Literal_string v -> Literal_string.pp ~gamma ppf v
  | `Name v -> Fmt.string ppf v

let rec pp ~gamma : string t Fmt.t =
 fun ppf -> function
  | Leaf s -> pp_s ~gamma ppf s
  | Add (a, b) -> Fmt.pf ppf "%a + %a" (pp ~gamma) a (pp ~gamma) b
  | Sub (a, b) -> Fmt.pf ppf "%a - %a" (pp ~gamma) a (pp ~gamma) b

type 'e t' =
  | Val : Uset.t -> 'e t'
  | Var : ('e, Uset.t ref) Var.t -> 'e t'
  | Add : 'e t' * 'e t' -> 'e t'
  | Sub : 'e t' * 'e t' -> 'e t'

type expr = Expr : 'e t' * 'e Gamma.t -> expr

let rec typ ~constants ~gamma expr =
  match expr with
  | Leaf (`Name name) -> (
      match Var.typ ~gamma name with
      | Var.V (x', g'', Ty.Grouping) -> Ok (Expr (Var x', g''))
      | Var.V (_, _, t') ->
          Error
            (Error.v expr gamma
               [ Error.Type_mismatch { a = Ty.V Ty.Grouping; b = Ty.V t' } ])
      | exception Var.Unbound_variable _ ->
          Error (Error.v expr gamma [ Error.Unbound_variable ]))
  | Leaf (`Literal_string v) ->
      let (Gamma.V g) = Gamma.typ gamma in
      let str = Literal_string.to_utf_8_string ~gamma:constants v in
      let set =
        Uutf.String.fold_utf_8
          (fun set _pos -> function
            | `Uchar uchar -> Uset.add uchar set
            | `Malformed _ -> set)
          Uset.empty str
      in
      Ok (Expr (Val set, g))
  | Add (a, b) -> (
      let (Gamma.V g0) = Gamma.typ gamma in
      match (typ ~constants ~gamma a, typ ~constants ~gamma b) with
      | Ok (Expr (Val a, g1)), Ok (Expr (Val b, g2)) -> (
          match (Gamma.equal g0 g1, Gamma.equal g1 g2) with
          | Some Refl.Refl, Some Refl.Refl ->
              Ok (Expr (Val (Uset.union a b), g0))
          | _ -> assert false)
      | Ok (Expr (a, g1)), Ok (Expr (b, g2)) -> (
          match (Gamma.equal g0 g1, Gamma.equal g1 g2) with
          | Some Refl.Refl, Some Refl.Refl -> Ok (Expr (Add (a, b), g0))
          | _ -> assert false)
      | Error err, _ -> Error err
      | _, Error err -> Error err)
  | Sub (a, b) -> (
      let (Gamma.V g0) = Gamma.typ gamma in
      match (typ ~constants ~gamma a, typ ~constants ~gamma b) with
      | Ok (Expr (Val a, g1)), Ok (Expr (Val b, g2)) -> (
          match (Gamma.equal g0 g1, Gamma.equal g1 g2) with
          | Some Refl.Refl, Some Refl.Refl ->
              Ok (Expr (Val (Uset.diff a b), g0))
          | _ -> assert false)
      | Ok (Expr (a, g1)), Ok (Expr (b, g2)) -> (
          match (Gamma.equal g0 g1, Gamma.equal g1 g2) with
          | Some Refl.Refl, Some Refl.Refl -> Ok (Expr (Sub (a, b), g0))
          | _ -> assert false)
      | Error err, _ -> Error err
      | _, Error err -> Error err)

let rec nth : type e a. e -> (e, a ref) Var.t -> a =
 fun gamma var ->
  match (var, gamma) with
  | Var.Z, (_rest, v) -> !v
  | Var.S n, (gamma, _) -> nth gamma n

let rec eval : type e. gamma:e -> e t' -> Uset.t =
 fun ~gamma -> function
  | Val uset -> uset
  | Var var -> nth gamma var
  | Add (a, b) ->
      let a = eval ~gamma a and b = eval ~gamma b in
      Uset.union a b
  | Sub (a, b) ->
      let a = eval ~gamma a and b = eval ~gamma b in
      Uset.diff a b
