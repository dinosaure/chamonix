[@@@warning "-30"]

type _ t =
  | Unit : unit t
  | Int : int t
  | Bool : bool t
  | String : string t
  | Routine : routine t

and routine = Routine

[@@@warning "+30"]

let pp : type a. a t Fmt.t =
 fun ppf -> function
  | Unit -> Fmt.string ppf "Unit"
  | Int -> Fmt.string ppf "Int"
  | Bool -> Fmt.string ppf "Bool"
  | String -> Fmt.string ppf "String"
  | Routine -> Fmt.string ppf "Routine"

type v = V : 'a t -> v
type value = Value : 'a t * 'a ref -> value

exception Type_mismatch of { a : v; b : v }

let equal : type a b. a t -> b t -> (a, b) Refl.t option =
 fun a b ->
  match (a, b) with
  | Unit, Unit -> Some Refl.Refl
  | Int, Int -> Some Refl.Refl
  | Bool, Bool -> Some Refl.Refl
  | String, String -> Some Refl.Refl
  | Routine, Routine -> Some Refl.Refl
  | _ -> None

let typ : [ `Unit | `Int | `Bool | `String ] -> v = function
  | `Unit -> V Unit
  | `Int -> V Int
  | `Bool -> V Bool
  | `String -> V String
