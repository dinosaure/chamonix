type t =
  | Strings of string list
  | Integers of string list
  | Booleans of string list
  | Routines of string list
  | Externals of string list
  | Groupings of string list

let pp ppf = function
  | Strings vs -> Fmt.pf ppf "strings(%a)" Fmt.(list ~sep:(any " ") string) vs
  | Integers vs -> Fmt.pf ppf "integers(%a)" Fmt.(list ~sep:(any " ") string) vs
  | Booleans vs -> Fmt.pf ppf "booleans(%a)" Fmt.(list ~sep:(any " ") string) vs
  | Routines vs -> Fmt.pf ppf "routines(%a)" Fmt.(list ~sep:(any " ") string) vs
  | Externals vs ->
      Fmt.pf ppf "externals(%a)" Fmt.(list ~sep:(any " ") string) vs
  | Groupings vs ->
      Fmt.pf ppf "groupings(%a)" Fmt.(list ~sep:(any " ") string) vs
