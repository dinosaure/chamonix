type t =
  | Unbound_variable
  | Type_mismatch of { a : Ty.v; b : Ty.v }
  | Gamma_mismatch of { g0 : Gamma.v; g1 : Gamma.v }

type 'expr v = Error of 'expr * Gamma.gamma * t list

let v expr gamma err = Error (expr, gamma, err)
let map_expr expr (Error (_expr, gamma, err)) = Error (expr, gamma, err)
