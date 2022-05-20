type t = {
  mutable cursor : int;
  mutable limit_forward : int;
  mutable limit_backward : int;
  mutable x : int;
  mutable y : int;
  mutable p : string;
}

let of_string str =
  {
    cursor = 0;
    limit_forward = String.length str;
    limit_backward = 0;
    x = 0;
    y = String.length str;
    p = str;
  }

let cursor t = t.cursor

let limit ~mode t =
  match mode with `Forward -> t.limit_forward | `Backward -> t.limit_backward

let hop ~mode t n =
  let o, c =
    match mode with `Forward -> (( + ), ( < )) | `Backward -> (( - ), ( > ))
  in
  if n >= 0 && c (o t.cursor n) (limit ~mode t) then (
    t.cursor <- o t.cursor n;
    true)
  else false

let utf_8_string_length str =
  let folder acc _idx = function
    | `Malformed str -> acc + String.length str
    | `Uchar _ -> succ acc
  in
  Uutf.String.fold_utf_8 folder 0 str

let len ~encoding t =
  match encoding with
  | `Byte -> String.length t.p
  | `UTF_8 -> utf_8_string_length t.p

let reset state old =
  state.cursor <- old.cursor;
  state.limit_forward <- old.limit_forward;
  state.limit_backward <- old.limit_backward;
  state.x <- old.x;
  state.y <- old.y;
  state.p <- old.p

let clone state =
  {
    cursor = state.cursor;
    limit_forward = state.limit_forward;
    limit_backward = state.limit_backward;
    x = state.x;
    y = state.y;
    p = state.p;
  }
