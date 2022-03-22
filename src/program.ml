type 'v t =
  | Declaration of Declaration.t
  | String_definition of string * Literal_string.t
  | Definition of string * 'v Command.t
  | Backward_mode of 'v t list
  | Grouping of
    { name : string
    ; x : [ `Literal_string of Literal_string.t | `Name of 'v ]
    ; r : ([ `Plus | `Minus ] * [ `Literal_string of Literal_string.t | `Name of 'v ]) list }

let gamma lst =
  let tbl = Hashtbl.create 0x10 in
  List.iter (function
    | String_definition (name, v) -> Hashtbl.add tbl name v
    | _ -> ()) lst ; tbl

let pp_s ~gamma ppf = function
  | `Literal_string v -> Literal_string.pp ~gamma ppf v
  | `Name v -> Fmt.string ppf v

let rec pp ~gamma ppf = function
  | Declaration v -> Declaration.pp ppf v
  | String_definition (name, v) ->
    Fmt.pf ppf "stringdef %s %a" name
      (Literal_string.pp ~gamma) v
  | Definition (name, command) ->
    Fmt.pf ppf "define %s as @[<hov 1>%a@]" name (Command.pp ~gamma) command
  | Backward_mode t ->
    Fmt.pf ppf "backwardmode @[<hov 1>(%a)@]" Fmt.(list ~sep:(any "@\n") (pp ~gamma)) t
  | Grouping { name; x; r= []; } ->
    Fmt.pf ppf "define %s %a" name (pp_s ~gamma) x
  | Grouping { name; x; r; } ->
    let pp_grouping ppf = function
      | (`Plus, v) -> Fmt.pf ppf "+%a" (pp_s ~gamma) v
      | (`Minus, v) -> Fmt.pf ppf "-%a" (pp_s ~gamma) v in
    Fmt.pf ppf "define %s %a @[<hov>%a@]"
      name (pp_s ~gamma) x Fmt.(list ~sep:(any "@ ") pp_grouping) r
