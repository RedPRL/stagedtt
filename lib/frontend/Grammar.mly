%{
open Command
open CS

let ap_or_atomic =
  function
  | [] -> failwith "Impossible Internal Error"
  | [f] -> f
  | f :: args -> Ap (f, args)

%}

%token <int> NUMERAL
%token <string> ATOM
(* Infix operators *)
%token COLON_EQUALS RIGHT_ARROW
(* Symbols *)
%token LAMBDA
(* Delimiters *)
%token LPR RPR
%token TYPE
(* Commands *)
%token DEF NORMALIZE STAGE PRINT FAIL QUIT
%token EOF

%start <Command.command list> commands
%type <CS.t>
  arrow
  atomic_term
  term

%%

commands:
  | EOF
    { [] }
  | cmd = command; cmds = commands
    { cmd :: cmds }

command:
  | DEF; ident = ATOM; tp = term; COLON_EQUALS tm = term
    { Declare {ident; tp = Some tp; tm} }
  | FAIL; ident = ATOM; tp = term; COLON_EQUALS tm = term
    { Declare {ident; tp = Some tp; tm} }
  | NORMALIZE; tm = term; 
    { Normalize {tm} }
  | STAGE; tm = term
    { Stage {tm} }
  | PRINT; nm = ATOM;
    { Print nm }
  | QUIT
    { Quit }

term:
  | tms = nonempty_list(atomic_term)
    { ap_or_atomic tms }
  | tm = arrow
    { tm }

arrow:
  | LAMBDA; nms = list(ATOM); RIGHT_ARROW; tm = term
    { Lam (nms, tm) }

atomic_term:
  | LPR; tm = term; RPR
    { tm }
  | nm = ATOM
    { Var nm }
  | TYPE; stage = NUMERAL
    { Univ { stage } }
