type 'v t =
  | Declaration of Declaration.t
  | String_definition of string * Literal_string.t
  | Definition of string * 'v Command.t
  | Backward_mode of 'v t list
  | Grouping of {
      name : string;
      x : [ `Literal_string of Literal_string.t | `Name of string ];
      r :
        ([ `Plus | `Minus ]
        * [ `Literal_string of Literal_string.t | `Name of string ])
        list;
    }

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

let rec index ~gamma : bound_variables -> de_bruijn_index = function
  | Declaration
      (Declaration.(
         ( Strings names
         | Integers names
         | Booleans names
         | Routines names
         | Externals names
         | Groupings names )) as decls) ->
      let top = Hashtbl.length gamma in
      List.iteri (fun index name -> Hashtbl.add gamma name (top + index)) names;
      Declaration decls
  | String_definition (name, str) -> String_definition (name, str)
  | Definition (name, command) ->
      let command = Command.index ~gamma command in
      let index = Hashtbl.length gamma in
      Hashtbl.add gamma name index;
      Definition (name, command)
  | Backward_mode prgms ->
      let prgms = List.map (index ~gamma) prgms in
      Backward_mode prgms
  | Grouping g -> Grouping g

let gamma :
    bound_variables list -> de_bruijn_index list * (string, int) Hashtbl.t =
 fun prgms ->
  let gamma = Hashtbl.create 0x100 in
  let prgms = List.map (index ~gamma) prgms in
  (prgms, gamma)

type ('e, 'a) t' =
  | Do : 'a Ty.t * ('e * 'a ref, unit) t' list -> ('e, unit) t'
  | Routine :
      string * ('e, 'a) Command.t' * ('e * Ty.routine, unit) t' list
      -> ('e, unit) t'
  | Backward_mod : ('e, 'a) t' list -> ('e, 'a list) t'
  | End : ('e, unit) t'

type expr = Expr : ('e, unit) t' list * 'e Gamma.t -> expr

let safe () = assert false
let always x _y = x

let rec typ ~constants ~gamma exprs =
  let go :
      gamma:Gamma.gamma ->
      de_bruijn_index list ->
      (expr, de_bruijn_index list Error.v) result =
   fun ~gamma exprs ->
    match exprs with
    | String_definition _ :: exprs -> typ ~constants ~gamma exprs
    | Grouping _ :: exprs -> typ ~constants ~gamma exprs
    | (Declaration (Declaration.Strings names) as expr) :: exprs -> (
        let (Gamma.V g0) =
          let rec go (Gamma.V gamma) = function
            | [] -> Gamma.V gamma
            | _name :: names -> go Gamma.(V (Ty.String :: gamma)) names
          in
          go (Gamma.typ gamma) names
        in
        let gamma = List.map (always `String) names @ gamma in
        match typ ~constants ~gamma exprs with
        | Ok (Expr (expr', g1)) -> (
            match Gamma.equal g0 g1 with
            | Some Refl.Refl ->
                let decls =
                  List.fold_left
                    (fun decls _name ->
                      match decls with
                      | Expr (decl, Ty.String :: gamma) ->
                          Expr ([ Do (Ty.String, decl) ], gamma)
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
    (* | (Definition (name, command) as expr) :: exprs -> (
        match Command.typ ~constants ~gamma command with
        | Ok (Command.Expr (command, gamma'')) ->
            Ok (Expr ([ Definition (name, command) ], gamma''))
        | Error err ->
            Error (Error.map_expr [ expr ] err)) *)
    | [] ->
        let (Gamma.V gamma) = Gamma.typ gamma in
        Ok (Expr ([ End ], gamma))
    | _ -> assert false
  in
  go ~gamma exprs

let rec eval :
    type e. gamma:e * e Gamma.t -> state:State.t -> (e, unit) t' list -> unit =
 fun ~gamma:(g, g') ~state exprs ->
  let rec go = function
    | Do (Ty.String, exprs) :: exprs' ->
        eval ~gamma:((g, ref ""), Gamma.(Ty.String :: g')) ~state exprs;
        go exprs'
    | [ End ] | [] -> ()
    | _ -> assert false
  in
  go exprs

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
  | Grouping { name; x; r = [] } ->
      Fmt.pf ppf "define %s %a" name (pp_s ~gamma) x
  | Grouping { name; x; r } ->
      let pp_grouping ppf = function
        | `Plus, v -> Fmt.pf ppf "+%a" (pp_s ~gamma) v
        | `Minus, v -> Fmt.pf ppf "-%a" (pp_s ~gamma) v
      in
      Fmt.pf ppf "define %s %a @[<hov>%a@]" name (pp_s ~gamma) x
        Fmt.(list ~sep:(any "@ ") pp_grouping)
        r
