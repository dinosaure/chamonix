type _ t = [] : unit t | ( :: ) : 'a Ty.t * 'gamma t -> ('gamma * 'a ref) t
type v = V : 'a t -> v
type gamma = [ `Unit | `Int | `Bool | `String | `Grouping ] list
(* XXX(dinosaure): de bruijn gamma environment *)

let rec typ : gamma -> v = function
  | [] -> V []
  | x :: r ->
      let (Ty.V x) = Ty.typ x in
      let (V r) = typ r in
      V (x :: r)

let rec equal : type a b. a t -> b t -> (a, b) Refl.t option =
 fun a b ->
  match (a, b) with
  | [], [] -> Some Refl.Refl
  | at :: a, bt :: b -> (
      match equal a b with
      | Some Refl.Refl -> (
          match Ty.equal at bt with
          | Some Refl.Refl -> Some Refl.Refl
          | None -> None)
      | None -> None)
  | _ -> None
