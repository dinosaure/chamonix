%token DOLLAR BRA KET MULTIPLY PLUS MINUS DIVIDE LS ASSIGN GR DEBUG LEFT_SLICE
  RIGHT_SLICE NE MULTIPLY_ASSIGN PLUS_ASSIGN MINUS_ASSIGN SLICE_TO COMMENT_2
  COMMENT_1 DIVIDE_ASSIGN INSERT SLICE_FROM LE EQ ASSIGN_TO GE AS DO OR AND FOR
  GET HEX HOP LEN NON NOT SET TRY FAIL GOTO LOOP NEXT SIZE TEST TRUE AMONG
  FALSE LENOF LIMIT UNSET ATMARK ATTACH CURSOR DEFINE DELETE GOPAST  MAXINT
  MININT REPEAT SIZEOF TOMARK ATLEAST ATLIMIT DECIMAL REVERSE SETMARK STRINGS
  TOLIMIT BOOLEANS INTEGERS ROUTINES SETLIMIT BACKWARDS EXTERNALS GROUPINGS
  STRINGDEF SUBSTRING BACKWARDMODE STRINGESCAPES

%token<int> NUMBER
%token<(char * char) * string> LITERAL_STRING
%token<string> NAME
%token<string> IDENTIFIER

%token EOF

%start<Program.t> strings

%%

let names(ty) == | ty; BRA; ~ = IDENTIFIER+; KET; < >

let strings   := | ~ = names(STRINGS);   <Program.Strings>
let booleans  := | ~ = names(BOOLEANS);  <Program.Booleans>
let integers  := | ~ = names(INTEGERS);  <Program.Integers>
let routines  := | ~ = names(ROUTINES);  <Program.Routines>
let externals := | ~ = names(EXTERNALS); <Program.Externals>
let groupings := | ~ = names(GROUPINGS); <Program.Groupings>

let expr :=
  | a = expr; PLUS; b = expr;  { Expr.Plus (a, b) }
  | a = expr; MINUS; b = expr; { Expr.Minus (a, b) }
  | ~ = LITERAL_STRING; <Expr.Literal_string>

let operation :=
  | ~ = routine; < >
  | a = operation; OR; b = operation;
    { Routine.Or (a, b) }
  | a = operation; AND; b = operation;
    { Routine.And (a, b) }

let injection(op, v) ==
  | name = IDENTIFIER; op; x = v; { (name, x) }

let routine :=
  | BRA; routines = operation; KET; < >
  | TEST; routine = routine; <Routine.Test>
  | REPEAT; routine = routine; <Routine.Repeat>
  | NEXT; <Routine.Next>
  | (name, v) = injection(SLICE_FROM, LITERAL_STRING);
    { Routine.Slice_from (name, v) }

let define :=
  | DEFINE; name = IDENTIFIER; expr = expr;
    { Program.Define_grouping (name, expr) }
  | DEFINE; name = IDENTIFIER; AS; routine = routine;
    { Program.Define_routine (name, routine) }

let instr :=
  | ~ = strings ; < >
  | ~ = booleans ; < >
  | ~ = integers ; < >
  | ~ = routines ; < >
  | ~ = externals ; < >
  | ~ = groupings ; < >

let program :=
  | ~ = instr+ ; EOF ; < >
