%{
%}

%token DOLLAR BRA KET MULTIPLY PLUS MINUS DIVIDE LS ASSIGN GR DEBUG LEFT_SLICE
  RIGHT_SLICE NE MULTIPLY_ASSIGN PLUS_ASSIGN MINUS_ASSIGN SLICE_TO
  DIVIDE_ASSIGN INSERT SLICE_FROM LE EQ ASSIGN_TO GE AS DO OR AND FOR
  GET HEX HOP LEN NON NOT SET TRY FAIL GOTO LOOP NEXT SIZE TEST TRUE AMONG
  FALSE LENOF LIMIT UNSET ATMARK ATTACH CURSOR DELETE GOPAST  MAXINT
  MININT REPEAT SIZEOF TOMARK ATLEAST ATLIMIT DECIMAL REVERSE SETMARK STRINGS
  TOLIMIT BOOLEANS INTEGERS ROUTINES SETLIMIT BACKWARDS EXTERNALS GROUPINGS
  SUBSTRING BACKWARDMODE

%token<int> NUMBER
%token<(char * char) * string> LITERAL_STRING
%token<string> NAME
%token<string> STRINGDEF
%token<string> DEFINE

%token EOF

%start<Program.t list> program

%left PLUS MINUS
%right OR AND
%right MULTIPLY DIVIDE

%nonassoc monadic

%%

let names(ty) == | ty; BRA; ~ = NAME*; KET; < >

let strings   := | ~ = names(STRINGS);   <Declaration.Strings>
let booleans  := | ~ = names(BOOLEANS);  <Declaration.Booleans>
let integers  := | ~ = names(INTEGERS);  <Declaration.Integers>
let routines  := | ~ = names(ROUTINES);  <Declaration.Routines>
let externals := | ~ = names(EXTERNALS); <Declaration.Externals>
let groupings := | ~ = names(GROUPINGS); <Declaration.Groupings>

let s :=
  | v = LITERAL_STRING; { `Literal_string v }
  | v = NAME; { `Name v }

let injection(op, v) ==
  | name = NAME; op; x = v; { (name, x) }

let arithmetic :=
  | ~ = NUMBER; <Arithmetic.Number>
  | ~ = NAME; <Arithmetic.Name>
  | MININT; { Arithmetic.Minint }
  | MAXINT; { Arithmetic.Maxint }
  | CURSOR; { Arithmetic.Cursor }
  | LIMIT; { Arithmetic.Limit }
  | SIZE; { Arithmetic.Size }
  | SIZEOF; ~ = s; <Arithmetic.Size_of>
  | LEN; { Arithmetic.Len }
  | LENOF; ~ = s; <Arithmetic.Len_of>
  | a = arithmetic; PLUS; b = arithmetic;
    { Arithmetic.Plus (a, b) }
  | a = arithmetic; MINUS; b = arithmetic;
    { Arithmetic.Minus (a, b) }
  | a = arithmetic; MULTIPLY; b = arithmetic;
    { Arithmetic.Multiply (a, b) }
  | a = arithmetic; DIVIDE; b = arithmetic;
    { Arithmetic.Divide (a, b) }

let among_string :=
  | literal_string = LITERAL_STRING ; name = NAME?;
    { `Search (literal_string, name) }
  | BRA; cs = command*; KET;
    { `Command cs }

let operator :=
  | EQ; { Test.Equal }
  | NE; { Test.Not_equal }
  | GR; { Test.Greater }
  | GE; { Test.Greater_equal }
  | LS; { Test.Less }
  | LE; { Test.Less_equal }

let test :=
  | DOLLAR; BRA; a = arithmetic; op = operator; b = arithmetic; KET;
    { Test.Test (a, op, b) }
  | DOLLAR; name = NAME; op = operator; a = arithmetic; 
    { Test.Test_from_gamma (name, op, a) }

let assign :=
  | DOLLAR; name = NAME; ASSIGN; v = arithmetic;
    { Command.Assign (name, v) }
  | DOLLAR; name = NAME; MULTIPLY_ASSIGN; v = arithmetic;
    { Command.Assign (name, Arithmetic.(Multiply (Name name, v))) }
  | DOLLAR; name = NAME; PLUS_ASSIGN; v = arithmetic;
    { Command.Assign (name, Arithmetic.(Plus (Name name, v))) }
  | DOLLAR; name = NAME; MINUS_ASSIGN; v = arithmetic;
    { Command.Assign (name, Arithmetic.(Minus (Name name, v))) }
  | DOLLAR; name = NAME; DIVIDE_ASSIGN; v = arithmetic;
    { Command.Assign (name, Arithmetic.(Divide (Name name, v))) }

let command :=
  | AMONG; BRA; vs = among_string*; KET;
    { Command.Among vs }
  | BRA; ~ = command*; KET; <Command.Commands>
  | a = command; OR; b = command; { Command.Or (a, b) }
  | a = command; AND; b = command; { Command.And (a, b) }
  | NOT; v = command; %prec monadic { Command.Not v }
  | TRY; v = command; %prec monadic { Command.Try v }
  | TEST; v = command; %prec monadic { Command.Test v }
  | FAIL; v = command; %prec monadic { Command.Fail v }
  | DO; v = command; %prec monadic { Command.Do v }
  | GOTO; v = command; %prec monadic { Command.Go_to v }
  | GOPAST; v = command; %prec monadic { Command.Go_past v }
  | REPEAT; v = command; %prec monadic { Command.Repeat v }
  | LOOP; n = arithmetic; v = command; %prec monadic { Command.Loop (n, v) }
  | ATLEAST; n = arithmetic; v = command; %prec monadic { Command.At_least (n, v) }
  | HOP; n = arithmetic; { Command.Hop n }
  | NON; MINUS; v = NAME; { Command.Non (Some `Minus, v) }
  | NON; v = NAME; { Command.Non (None, v) }
  | SLICE_FROM; ~ = s; <Command.Replace_slice>
  | SLICE_TO; ~ = s; <Command.Move_slice>
  | SETMARK; ~ = NAME; <Command.Set_mark>
  | LEFT_SLICE ; { Command.Left_end }
  | RIGHT_SLICE ; { Command.Right_end }
  | ATMARK; ~ = arithmetic; <Command.At_mark>
  | NEXT; { Command.Next }
  | DELETE; { Command.Delete }
  | BACKWARDS; ~ = command; %prec monadic <Command.Backwards>
  | DOLLAR; name = NAME; command = command; %prec monadic { Command.String (name, command) }
  | SET; ~ = NAME; <Command.Set>
  | UNSET; ~ = NAME; <Command.Unset>
  | SETLIMIT; a = command; FOR; b = command; %prec monadic { Command.Set_limit (a, b) }
  | TOMARK; ~ = arithmetic; <Command.To_mark>
  | SUBSTRING; { Command.Substring }
  | ASSIGN_TO; ~ = NAME; <Command.Assign_to>
  | ATLIMIT; { Command.At_limit }
  | TOLIMIT; { Command.To_limit }
  | INSERT; ~ = s; <Command.Insert>
  | TRUE; { Command.True }
  | FALSE; { Command.False }
  | ATTACH; ~ = s; <Command.Attach>
  | ~ = test; <Command.Arithmetic_test>
  | ~ = s; <Command.S>
  | ~ = assign; < >

let declaration :=
  | ~ = strings ; < >
  | ~ = integers ; < >
  | ~ = booleans ; < >
  | ~ = routines ; < >
  | ~ = externals ; < >
  | ~ = groupings ; < >

let grouping :=
  | PLUS; v = s; { `Plus, v }
  | MINUS; v = s; { `Minus, v }

let p := 
  | v = declaration; { Program.Declaration v }
  | name = DEFINE; AS; command = command; { Program.Definition (name, command) }
  | BACKWARDMODE; BRA; v = p*; KET; { Program.Backward_mode v }
  | name = STRINGDEF; v = LITERAL_STRING; { Program.String_definition (name, v) }
  | name = DEFINE; x = s; r = grouping*; { Program.Grouping { name; x; r; }}

let program :=
  | ~ = p* ; EOF ; < >
