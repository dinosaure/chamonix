type t = { escape : char * char; contents : string }

let v ~escape contents = { escape; contents }
let empty = { escape = ('{', '}'); contents = "" }

let split ~escape str =
  let max = String.length str in
  let extract idx =
    let len = ref 0 in
    while str.[idx + !len] <> snd escape do
      incr len
    done;
    (idx + !len + 1, `Expand (String.sub str idx !len))
  in
  let rec go anchor idx acc =
    if idx >= max then
      if anchor = idx then List.rev acc
      else List.rev (`String (String.sub str anchor (idx - anchor)) :: acc)
    else if str.[idx] = fst escape then
      let idx', sub = extract (succ idx) in
      if anchor = idx then go idx' idx' (sub :: acc)
      else
        let sub' = `String (String.sub str anchor (idx - anchor)) in
        go idx' idx' (sub :: sub' :: acc)
    else go anchor (succ idx) acc
  in
  go 0 0 []

let is_string = function `String _ -> true | _ -> false
let safe () = assert false

let rec to_utf_8_string ~gamma ~escape str =
  let lst = split ~escape str in
  let map = function
    | `Expand str when Hashtbl.mem gamma str ->
        let { escape; contents } = Hashtbl.find gamma str in
        let str = to_utf_8_string ~gamma ~escape contents in
        `String str
    | `Expand "{" -> `String "{"
    | `Expand "'" -> `String "'"
    | `Expand str -> (
        try
          let cp = Unicode.unicode (Lexing.from_string str) in
          let bf = Buffer.create 4 in
          Uutf.Buffer.add_utf_8 bf cp;
          `String (Buffer.contents bf)
        with _ -> `Not_found str)
    | `String _ as v -> v
  in
  let lst = List.map map lst in
  match List.partition is_string lst with
  | lst, [] ->
      String.concat "" (List.map (function `String v -> v | _ -> safe ()) lst)
  | _, rest ->
      let pp ppf = function
        | `Expand str -> Fmt.pf ppf "(@[<1>`Expand@ %S@])" str
        | `Not_found str -> Fmt.pf ppf "(@[<1>`Not_found %S@])" str
        | `String _ -> safe ()
      in
      Fmt.failwith "The string %S can not be compiled! Errors with: %a." str
        Fmt.(Dump.list pp)
        rest

let to_utf_8_string ~gamma { escape; contents } =
  to_utf_8_string ~gamma ~escape contents

let pp ~gamma ppf v = Fmt.string ppf (to_utf_8_string ~gamma v)
