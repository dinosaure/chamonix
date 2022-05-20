type point = { lnum : int; cnum : int; seek : int }
type t = point * point

let pp_point ppf { lnum; cnum; seek = _ } = Fmt.pf ppf "l.%d@ c.%d" lnum cnum

let pp ppf (a, b) =
  let pp prefix ppf (a, b) =
    if a = b then Fmt.pf ppf "%s%d" prefix a
    else Fmt.pf ppf "%s%d - %d" prefix a b
  in
  Fmt.pf ppf "%a %a" (pp "l.") (a.lnum, b.lnum) (pp "c.") (a.cnum, b.cnum)

let point lexbuf =
  {
    lnum = lexbuf.Lexing.pos_lnum;
    cnum = lexbuf.Lexing.pos_cnum - lexbuf.Lexing.pos_bol;
    seek = lexbuf.Lexing.pos_cnum;
  }

let make a b = (point a, point b)
