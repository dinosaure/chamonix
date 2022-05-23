type ('e, 'a) t = Z : ('e * 'a, 'a) t | S : ('e, 'a) t -> ('e * 'b, 'a) t

exception Unbound_variable of Gamma.gamma

type v = V : ('e, 'a ref) t * 'e Gamma.t * 'a Ty.t -> v

let rec to_int : type e a. (e, a) t -> int = function
  | Z -> 0
  | S v -> succ (to_int v)

let rec of_int : type e. gamma:e Gamma.t -> int -> (v, unit) result =
 fun ~gamma n ->
  match (gamma, n) with
  | Gamma.[], _ -> assert false
  | Gamma.(t :: _), 0 -> Ok (V (Z, gamma, t))
  | Gamma.(_ :: r), n -> (
      match of_int ~gamma:r (pred n) with
      | Ok (V (n, r', t')) -> (
          match Gamma.equal r r' with
          | Some Refl.Refl -> Ok (V (S n, gamma, t'))
          | None -> assert false)
      | Error _ -> assert false)

let rec typ : gamma:Gamma.gamma -> int -> v =
 fun ~gamma v ->
  match (v, gamma) with
  | 0, t :: r ->
      let (Ty.V t) = Ty.typ t in
      let (Gamma.V r) = Gamma.typ r in
      V (Z, t :: r, t)
  | n, t :: r ->
      let (V (x', r', tr)) = typ ~gamma:r (n - 1) in
      let (Ty.V t') = Ty.typ t in
      V (S x', t' :: r', tr)
  | _, gamma -> raise (Unbound_variable gamma)
