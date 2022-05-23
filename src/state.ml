type t = {
  mutable cursor : int;
  mutable limit : int;
  mutable x : int;
  mutable y : int;
  mutable p : string;
}

let of_string str =
  {
    cursor = 0;
    limit = String.length str;
    x = 0;
    y = String.length str;
    p = str;
  }

let cursor t = t.cursor
let limit ~mode:_ t = t.limit

let hop ~mode t n =
  let op, cmp =
    match mode with `Forward -> (( + ), ( > )) | `Backward -> (( - ), ( < ))
  in
  if n >= 0 && cmp (op t.cursor n) t.limit then false
  else (
    t.cursor <- op t.cursor n;
    true)

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
  state.limit <- old.limit;
  state.x <- old.x;
  state.y <- old.y;
  state.p <- old.p

let clone state =
  {
    cursor = state.cursor;
    limit = state.limit;
    x = state.x;
    y = state.y;
    p = state.p;
  }
