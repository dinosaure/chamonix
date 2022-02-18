type t =
  | Define_as of string * Command.t

let pp ppf = function
  | Define_as (name, command) ->
    Fmt.pf ppf "define %s as @[<hov>%a@]" name Command.pp command
