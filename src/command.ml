type 'v t =
  | (* * *) Or of 'v t * 'v t
  | (* * *) And of 'v t * 'v t
  | (* * *) Test of 'v t
  | (* * *) Go_to of 'v t
  | (* * *) Go_past of 'v t
  | (* * *) Repeat of 'v t
  | (* * *) Set_mark of 'v
  | (* * *) At_mark of 'v Arithmetic.t
  | (* * *) To_mark of 'v Arithmetic.t
  | (* * *) Loop of 'v Arithmetic.t * 'v t
  | (* * *) Hop of 'v Arithmetic.t
  | Non of [ `Minus ] option * string
  | (* * *) Replace_slice of [ `Literal_string of Literal_string.t | `Name of 'v ]
  | (* * *) Move_slice of 'v
  | (* * *) At_limit
  | (* * *) To_limit
  | (* * *) Set of 'v
  | (* * *) Unset of 'v
  | (* * *) Left_end
  | (* * *) Right_end
  | (* * *) True
  | (* * *) False
  | (* * *) Assign_to of 'v
  | (* * *) Commands of 'v t list
  | Among of [ `Search of Literal_string.t * 'v option
             | `Commands of 'v t list ] list
  | Backwards of 'v t
  | (* * *) String_command of 'v * 'v t
  | (* * *) Arithmetic_test of 'v Test.t
  | (* * *) Assign of 'v * [ `Arithmetic of 'v Arithmetic.t
                           | `Literal_string of Literal_string.t
                           | `Name of 'v ]
  | (* * *) S of [ `Literal_string of Literal_string.t | `Name of 'v ]
  | (* * *) Insert of [ `Literal_string of Literal_string.t | `Name of 'v ]
  | (* * *) Attach of [ `Literal_string of Literal_string.t | `Name of 'v ]
  | (* * *) Set_limit of 'v t * 'v t
  | Substring

type de_bruijn_index = int t
type bound_variables = string t

type ('e, 'a) t' =
  | Bin : [ `And | `Or ] * ('e, bool) t' * ('e, bool) t' -> ('e, bool) t'
  | Var : ('e, 'a) Var.t -> ('e, 'a) t'
  | Val : 'a Ty.t * 'a -> ('e, 'a) t'
  | Hop : ('e, int) t' -> ('e, bool) t'
  | For : 'e Arithmetic.t' * ('e, bool) t' list -> ('e, bool) t'
  | Seq : ('e, bool) t' list -> ('e, bool) t'
  | Test : 'e Test.t -> ('e, bool) t'
  | At_limit : ('e, bool) t'
  | To_limit : ('e, bool) t'
  | At_mark : ('e, int) t' -> ('e, bool) t'
  | To_mark : ('e, int) t' -> ('e, bool) t'
  | Repeat : ('e, bool) t' -> ('e, bool) t'
  | Assign : ('e, 'a) Var.t * ('e, 'a) t' -> ('e, bool) t'
  | Assign_to : ('e, string) Var.t * ('e, string) Var.t -> ('e, bool) t'
  | Insert : ('e, string) t' -> ('e, bool) t'
  | Attach : ('e, string) t' -> ('e, bool) t'
  | Set_mark : ('e, int) Var.t -> ('e, bool) t'
  | Set_slice : [ `L | `R | `Replace of ('e, string) t' ] -> ('e, bool) t'
  | Get_slice : ('e, string) t' -> ('e, bool) t'
  | Set_limit : ('e, bool) t' * ('e, bool) t' -> ('e, bool) t'
  | Go_to : ('e, bool) t' -> ('e, bool) t'
  | Go_past : ('e, bool) t' -> ('e, bool) t'
  | String_command : ('e, bool) t' -> ('e, unit) t'

type expr = Expr : ('e, bool) t' * 'e Gamma.t -> expr

exception Unbound_variable of string

let rec index ~gamma : bound_variables -> de_bruijn_index = function
  | Or (a, b) -> Or (index ~gamma a, index ~gamma b)
  | And (a, b) -> And (index ~gamma a, index ~gamma b)
  | Test v -> Test (index ~gamma v)
  | Go_to v -> Go_to (index ~gamma v)
  | Go_past v -> Go_past (index ~gamma v)
  | Repeat v -> Repeat (index ~gamma v)
  | Set_mark name ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Set_mark index
    | None -> raise (Unbound_variable name) )
  | At_mark v -> At_mark (Arithmetic.index ~gamma v)
  | To_mark v -> To_mark (Arithmetic.index ~gamma v)
  | Loop (n, v) -> Loop (Arithmetic.index ~gamma n, index ~gamma v)
  | Hop v -> Hop (Arithmetic.index ~gamma v)
  | Non (minus, v) -> Non (minus, v)
  | Replace_slice (`Name name) ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Replace_slice (`Name index)
    | None -> raise (Unbound_variable name) )
  | Replace_slice (`Literal_string _ as v) ->
    Replace_slice v
  | Move_slice name ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Move_slice index
    | None -> raise (Unbound_variable name) )
  | At_limit -> At_limit
  | To_limit -> To_limit
  | Set name -> 
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Set index
    | None -> raise (Unbound_variable name) )
  | Unset name ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Unset index
    | None -> raise (Unbound_variable name) )
  | Left_end -> Left_end
  | Right_end -> Right_end
  | True -> True
  | False -> False
  | Assign_to name ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Assign_to index
    | None -> raise (Unbound_variable name) )
  | Commands lst ->
    Commands (List.map (index ~gamma) lst)
  | String_command (name, v) ->
    ( match Hashtbl.find_opt gamma name with
    | Some idx -> String_command (idx, index ~gamma v)
    | None -> raise (Unbound_variable name) )
  | Arithmetic_test v ->
    Arithmetic_test (Test.index ~gamma v)
  | Assign (name, `Name name') ->
    ( match Hashtbl.find_opt gamma name, Hashtbl.find_opt gamma name' with
    | Some index, Some index'-> Assign (index, `Name index')
    | Some _, None -> raise (Unbound_variable name')
    | None, _ -> raise (Unbound_variable name) )
  | Assign (name, (`Arithmetic v)) ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Assign (index, `Arithmetic (Arithmetic.index ~gamma v))
    | None -> raise (Unbound_variable name) )
  | Assign (name, (`Literal_string _ as v)) ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Assign (index, v) 
    | None -> raise (Unbound_variable name) )
  | S (`Literal_string _ as v) -> S v
  | S (`Name name) ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> S (`Name index)
    | None -> raise (Unbound_variable name) )
  | Insert (`Literal_string _ as v) -> Insert v
  | Insert (`Name name) ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Insert (`Name index)
    | None -> raise (Unbound_variable name) )
  | Attach (`Literal_string _ as v) -> Attach v
  | Attach (`Name name) ->
    ( match Hashtbl.find_opt gamma name with
    | Some index -> Attach (`Name index)
    | None -> raise (Unbound_variable name) )
  | Set_limit (a, b) -> Set_limit (index ~gamma a, index ~gamma b)
  | Backwards v -> Backwards (index ~gamma v)
  | Among lst ->
    let f = function
      | `Search (v, Some name) ->
        ( match Hashtbl.find_opt gamma name with
        | Some index -> `Search (v, Some index)
        | None -> raise (Unbound_variable name) )
      | `Search (v, None) -> `Search (v, None)
      | `Commands lst -> `Commands (List.map (index ~gamma) lst) in
    Among (List.map f lst)
  | Substring -> Substring

type s =
  [ `Name of string
  | `Literal_string of Literal_string.t ]

let pp_s ~gamma ppf = function
  | `Literal_string v -> Literal_string.pp ~gamma ppf v
  | `Name v -> Fmt.string ppf v

let rec pp ~gamma ppf = function
  | Or (a, b) -> Fmt.pf ppf "%a@ or@ %a" (pp ~gamma) a (pp ~gamma) b
  | And (a, b) -> Fmt.pf ppf "%a@ and@ %a" (pp ~gamma) a (pp ~gamma) b
  | Test t -> Fmt.pf ppf "test %a" (pp ~gamma) t
  | Go_to t -> Fmt.pf ppf "goto %a" (pp ~gamma) t
  | Go_past t -> Fmt.pf ppf "gopast %a" (pp ~gamma) t
  | Repeat t -> Fmt.pf ppf "repeat %a" (pp ~gamma) t
  | Loop (a, t) -> Fmt.pf ppf "loop %a %a" (Arithmetic.pp ~gamma) a (pp ~gamma) t
  | Hop a -> Fmt.pf ppf "hop %a" (Arithmetic.pp ~gamma) a
  | Left_end -> Fmt.string ppf "["
  | Right_end -> Fmt.string ppf "]"
  | Set_mark name -> Fmt.pf ppf "setmark %s" name
  | Commands lst -> Fmt.pf ppf "@[<hov 1>(%a)@]" Fmt.(list ~sep:(any "@ ") (pp ~gamma)) lst
  | Among lst ->
    let pp_line ppf = function
      | `Search (literal_string, Some name) ->
        Fmt.pf ppf "%a %s" (Literal_string.pp ~gamma) literal_string name
      | `Search (literal_string, None) ->
        Literal_string.pp ~gamma ppf literal_string
      | `Commands cs ->
        Fmt.pf ppf "(%a)" Fmt.(list ~sep:(any "@ ") (pp ~gamma)) cs in
    Fmt.pf ppf "among(@[<1>%a@])"
      Fmt.(list ~sep:(any "@ ") pp_line) lst
  | Replace_slice v -> Fmt.pf ppf "<- %a" (pp_s ~gamma) v
  | True -> Fmt.string ppf "true"
  | False -> Fmt.string ppf "false"
  | Backwards t -> Fmt.pf ppf "backwards %a" (pp ~gamma) t
  | String_command (name, t) -> Fmt.pf ppf "$%s %a" name (pp ~gamma) t
  | Arithmetic_test test -> Test.pp ~gamma ppf test
  | Set name -> Fmt.pf ppf "set %s" name
  | Unset name -> Fmt.pf ppf "unset %s" name
  | S v -> pp_s ~gamma ppf v
  | Assign (name, (#s as v)) ->
    Fmt.pf ppf "$%s = %a" name (pp_s ~gamma) v
  | Assign (name, `Arithmetic v) ->
    Fmt.pf ppf "$%s = %a" name (Arithmetic.pp ~gamma) v
  | Non (Some `Minus, name) -> Fmt.pf ppf "non -%s" name
  | Non (None, name) -> Fmt.pf ppf "non %s" name
  | Set_limit (a, b) -> Fmt.pf ppf "setlimit %a for %a" (pp ~gamma) a (pp ~gamma) b
  | To_mark v -> Fmt.pf ppf "tomark %a" (Arithmetic.pp ~gamma) v
  | Substring -> Fmt.string ppf "substring"
  | Assign_to v -> Fmt.pf ppf "=> %s" v
  | Move_slice name -> Fmt.pf ppf "-> %s" name
  | At_limit -> Fmt.string ppf "atlimit"
  | To_limit -> Fmt.string ppf "tolimit"
  | Insert v -> Fmt.pf ppf "insert %a" (pp_s ~gamma) v
  | At_mark v -> Fmt.pf ppf "atmark %a" (Arithmetic.pp ~gamma) v
  | Attach v -> Fmt.pf ppf "attach %a" (pp_s ~gamma) v
