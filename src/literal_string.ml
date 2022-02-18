type t = (char * char) * string

let pp ppf ((_sa, _sb), str) = Fmt.pf ppf "%S" str
