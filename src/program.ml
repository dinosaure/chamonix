type t =
  | Declaration of Declaration.t
  | String_definition of string * Literal_string.t
  | Definition of string * Command.t
  | Backward_mode of t list
  | Grouping of
    { name : string
    ; x : [ `Literal_string of Literal_string.t | `Name of string ]
    ; r : ([ `Plus | `Minus ] * [ `Literal_string of Literal_string.t | `Name of string ]) list }

let pp_s ppf = function
  | `Literal_string v -> Literal_string.pp ppf v
  | `Name v -> Fmt.string ppf v

let rec pp ppf = function
  | Declaration v -> Declaration.pp ppf v
  | String_definition (name, v) ->
    Fmt.pf ppf "stringdef@ %s@ %a" name
      Literal_string.pp v
  | Definition (name, command) ->
    Fmt.pf ppf "define %s as %a" name Command.pp command
  | Backward_mode t ->
    Fmt.pf ppf "backwardmode (@[<1>%a@])" Fmt.(list ~sep:(any "@ ") pp) t
  | Grouping { name; x; r= []; } ->
    Fmt.pf ppf "define %s %a" name pp_s x
  | Grouping { name; x; r; } ->
    let pp_grouping ppf = function
      | (`Plus, v) -> Fmt.pf ppf "+%a" pp_s v
      | (`Minus, v) -> Fmt.pf ppf "-%a" pp_s v in
    Fmt.pf ppf "define %s %a %a"
      name pp_s x Fmt.(list ~sep:(any "@ ") pp_grouping) r
