type 'v t =
  | Declaration of Declaration.t
  | String_definition of string * Literal_string.t
  | Definition of string * 'v Command.t
  | Backward_mode of 'v t list
  | Grouping of 'v * 'v Grouping.t

type de_bruijn_index = int t
type bound_variables = string t

let constants : 'v t list -> (string, Literal_string.t) Hashtbl.t =
 fun prgms ->
  let tbl = Hashtbl.create 0x100 in
  List.iter
    (function
      | String_definition (name, str) -> Hashtbl.add tbl name str | _ -> ())
    prgms;
  tbl

exception Unbound_variable of string

let rec index ~gamma : bound_variables -> de_bruijn_index = function
  | Declaration
      (Declaration.(
         ( Strings names
         | Integers names
         | Booleans names
         | Routines names
         | Groupings names )) as decls) ->
      let top = Hashtbl.length gamma in
      List.iteri (fun index name -> Hashtbl.add gamma name (top + index)) names;
      Declaration decls
  | Declaration (Declaration.Externals _names as decls) -> Declaration decls
  | String_definition (name, str) -> String_definition (name, str)
  | Definition (name, command) ->
      let command = Command.index ~gamma command in
      let index = Hashtbl.length gamma in
      Hashtbl.add gamma name index;
      Definition (name, command)
  | Backward_mode prgms ->
      let prgms = List.map (index ~gamma) prgms in
      Backward_mode prgms
  | Grouping (name, g) -> (
      match Hashtbl.find_opt gamma name with
      | Some ridx ->
          Grouping (Hashtbl.length gamma - 1 - ridx, Grouping.index ~gamma g)
      | None -> raise (Unbound_variable name))

let gamma :
    bound_variables list -> de_bruijn_index list * (string, int) Hashtbl.t =
 fun prgms ->
  let gamma = Hashtbl.create 0x100 in
  let prgms = List.map (index ~gamma) prgms in
  (prgms, gamma)

type ('e, 'a) t' =
  | Do : 'a Ty.t * ('e * 'a ref, unit) t' list -> ('e, unit) t'
  | Set_grouping :
      ('e, Uset.t ref) Var.t * 'e Grouping.t' * ('e, 'a) t' list
      -> ('e, 'a) t'
  | Routine :
      routine * mode * ('e, bool) Command.t' * ('e, unit) t' list
      -> ('e, unit) t'
  | Backward_mod : ('e, 'a) t' list -> ('e, 'a list) t'
  | End : ('e, unit) t'

and routine = string
and mode = [ `Forward | `Backward ]
(* XXX(dinosaure): in our language, [Routine] is **not** a first class citizen
 * value as a function [unit -> unit] but a jump. The langauge is not powerful
 * enough to keep pointers to function, so [typ] just try to type body of the
 * routine and continue the process to the [End]. *)

type expr = Expr : ('e, unit) t' list * 'e Gamma.t -> expr

let safe () = assert false
let always x _y = x

let externals exprs =
  let module Set = Set.Make (String) in
  let rec go acc = function
    | [] -> acc
    | Declaration (Declaration.Externals names) :: exprs ->
        let acc = List.fold_left (fun acc x -> Set.add x acc) acc names in
        go acc exprs
    | _ :: exprs -> go acc exprs
  in
  Set.elements (go Set.empty exprs)

let typ ~constants ~gamma exprs =
  let rec go :
      gamma:Gamma.gamma ->
      mode:[ `Forward | `Backward ] ->
      de_bruijn_index list ->
      (expr, de_bruijn_index list Error.v) result =
   fun ~gamma ~mode exprs ->
    match exprs with
    | String_definition _ :: exprs -> go ~gamma ~mode exprs
    | Grouping (idx, g) :: exprs -> (
        let (Gamma.V g0) = Gamma.typ gamma in
        match
          ( Var.of_int ~gamma:g0 idx,
            Grouping.typ ~constants ~gamma g,
            go ~gamma ~mode exprs )
        with
        | ( Ok (Var.V (var, g1, Ty.Grouping)),
            Ok (Grouping.Expr (g, g2)),
            Ok (Expr (exprs, g3)) ) -> (
            match (Gamma.equal g0 g1, Gamma.equal g1 g2, Gamma.equal g2 g3) with
            | Some Refl.Refl, Some Refl.Refl, Some Refl.Refl ->
                Ok (Expr ([ Set_grouping (var, g, exprs) ], g1))
            | _ -> assert false)
        | _, _, (Error _ as err) -> err
        | Error _, Error _, Ok _ -> assert false
        | Ok (Var.V (_, _, ty)), _, _ ->
            Fmt.failwith "Var typed with something else (idx: %d): %a" idx Ty.pp
              ty
        | Error _, Ok _, _ -> failwith "Var failed but grouping typed.")
    | Declaration (Declaration.Externals _names) :: exprs ->
        go ~gamma ~mode exprs
    | (Declaration decls as expr) :: exprs -> (
        let names = Declaration.names decls in
        let v, Ty.V typ' = Declaration.typ decls in
        let (Gamma.V g0) =
          let rec go (Gamma.V gamma) = function
            | [] -> Gamma.V gamma
            | _name :: names -> go Gamma.(V (typ' :: gamma)) names
          in
          go (Gamma.typ gamma) names
        in
        let gamma = List.map (always v) names @ gamma in
        match go ~gamma ~mode exprs with
        | Ok (Expr (expr', g1)) -> (
            match Gamma.equal g0 g1 with
            | Some Refl.Refl ->
                let decls =
                  List.fold_left
                    (fun decls _name ->
                      match decls with
                      | Expr (decl, typ'' :: gamma) -> (
                          match Ty.equal typ' typ'' with
                          | Some Refl.Refl -> Expr ([ Do (typ', decl) ], gamma)
                          | None -> safe ())
                      | _ -> safe ())
                    (Expr (expr', g0))
                    names
                in
                Ok decls
            | None ->
                Error
                  (Error.v exprs gamma
                     [
                       Error.Gamma_mismatch { g0 = Gamma.V g0; g1 = Gamma.V g1 };
                     ]))
        | Error err -> Error (Error.map_expr [ expr ] err))
    | (Definition (name, command) as expr) :: exprs -> (
        match
          (Command.typ ~constants ~gamma command, go ~gamma ~mode exprs)
        with
        | Ok (Command.Expr (command, g0)), Ok (Expr (exprs', g1)) -> (
            match Gamma.equal g0 g1 with
            | Some Refl.Refl ->
                Ok (Expr ([ Routine (name, mode, command, exprs') ], g1))
            | _ ->
                Error
                  (Error.v exprs gamma
                     [
                       Error.Gamma_mismatch { g0 = Gamma.V g0; g1 = Gamma.V g1 };
                     ]))
        | Error err, _ -> Error (Error.map_expr [ expr ] err)
        | _, Error err -> Error err)
    | Backward_mode exprs :: exprs' -> (
        let (Gamma.V g0) = Gamma.typ gamma in
        match go ~mode:`Backward ~gamma exprs with
        | Ok (Expr (exprs, g1)) -> (
            match go ~mode ~gamma exprs' with
            | Ok (Expr (exprs', g2)) -> (
                match (Gamma.equal g0 g1, Gamma.equal g1 g2) with
                | Some Refl.Refl, Some Refl.Refl ->
                    Ok (Expr (exprs @ exprs', g2))
                | _ -> assert false (* TODO *))
            | Error _ as err -> err)
        | Error _ as err -> err)
    | [] ->
        let (Gamma.V gamma) = Gamma.typ gamma in
        Ok (Expr ([ End ], gamma))
  in
  go ~mode:`Forward ~gamma exprs

let rec set : type e a. e -> (e, a ref) Var.t -> a -> unit =
 fun gamma var v ->
  match (var, gamma) with
  | Var.Z, (_rest, value) -> value := v
  | Var.S n, (gamma, _) -> set gamma n v

let eval :
    type e0.
    routine:string ->
    gamma:e0 * e0 Gamma.t ->
    state:State.t ->
    (e0, unit) t' list ->
    unit =
 fun ~routine ~gamma ~state exprs ->
  let rec go :
      type e1.
      gamma:e1 * e1 Gamma.t -> state:State.t -> (e1, unit) t' list -> unit =
   fun ~gamma:((g, g') as gamma) ~state -> function
    | Do (Ty.String, exprs) :: exprs' ->
        go ~gamma:((g, ref ""), Gamma.(Ty.String :: g')) ~state exprs;
        go ~gamma ~state exprs'
    | Do (Ty.Grouping, exprs) :: exprs' ->
        go ~gamma:((g, ref Uset.empty), Gamma.(Ty.Grouping :: g')) ~state exprs;
        go ~gamma ~state exprs'
    | Routine (routine', mode, cmd, _expr) :: exprs' when routine = routine' ->
        let _ (* TODO *) = Command.eval ~gamma ~mode ~state cmd in
        go ~gamma ~state exprs'
    | Set_grouping (var, uset, exprs) :: exprs' ->
        let uset = Grouping.eval ~gamma:g uset in
        set g var uset;
        let _ (* TODO *) = go ~gamma ~state exprs in
        go ~gamma ~state exprs'
    | [ End ] | [] -> ()
    | _ -> assert false
  in
  go ~gamma ~state exprs

let pp_s ~gamma ppf = function
  | `Literal_string v -> Literal_string.pp ~gamma ppf v
  | `Name v -> Fmt.string ppf v

let rec pp ~gamma : bound_variables Fmt.t =
 fun ppf -> function
  | Declaration v -> Declaration.pp ppf v
  | String_definition (name, v) ->
      Fmt.pf ppf "stringdef %s %a" name (Literal_string.pp ~gamma) v
  | Definition (name, command) ->
      Fmt.pf ppf "define %s as @[<hov 1>%a@]" name (Command.pp ~gamma) command
  | Backward_mode t ->
      Fmt.pf ppf "backwardmode @[<hov 1>(%a)@]"
        Fmt.(list ~sep:(any "@\n") (pp ~gamma))
        t
  | Grouping (name, g) -> Fmt.pf ppf "define %s %a" name (Grouping.pp ~gamma) g
