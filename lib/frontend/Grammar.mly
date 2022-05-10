%{
open Prelude
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
%token COLON COLON_COLON COLON_EQUALS RIGHT_ARROW UNDERSCORE
(* Symbols *)
%token LAMBDA
(* Delimiters *)
%token LPR RPR LSQ RSQ UP_LSQ DOWN_LSQ DOUBLE_UP_LSQ
(* Keywords *)
%token TYPE THE
(* Commands *)
%token DEF NORMALIZE STAGE PRINT FAIL QUIT
%token EOF

%start <Command.command list> commands
%type <CS.t>
  arrow
  atomic_term
  term
%type <Ident.path>
  path
%type <Ident.t>
  name

%right RIGHT_ARROW

%%

path:
  | path = separated_nonempty_list(COLON_COLON, ATOM)
    { path }

name:
  | path = path
    { User path }
  | UNDERSCORE
    { Anon }

commands:
  | EOF
    { [] }
  | cmd = command; cmds = commands
    { cmd :: cmds }

command:
  | DEF; ident = name; COLON; tp = term; COLON_EQUALS tm = term
    { Declare {ident; tp = Some tp; tm} }
  | FAIL; ident = name; tp = term; COLON_EQUALS tm = term
    { Declare {ident; tp = Some tp; tm} }
  | NORMALIZE; tm = term; 
    { Normalize {tm} }
  | STAGE; tm = term
    { Stage {tm} }
  | PRINT; nm = name;
    { Print nm }
  | QUIT
    { Quit }

term:
  | tms = nonempty_list(atomic_term)
    { ap_or_atomic tms }
  | tm = arrow
    { tm }

arrow:
  | LAMBDA; nms = list(name); RIGHT_ARROW; tm = term
    { Lam (nms, tm) }
  | LPR; ident = name; COLON; base = term; RPR; RIGHT_ARROW; fam = term
    { Pi (base, ident, fam) }
  | base = term; RIGHT_ARROW; fam = term
    { Pi (base, Anon, fam) }

atomic_term:
  | LPR; tm = term; RPR
    { tm }
  | DOUBLE_UP_LSQ; tm = term; RSQ
    { Expr tm }
  | UP_LSQ; tm = term; RSQ
    { Quote tm }
  | DOWN_LSQ; tm = term; RSQ
    { Splice tm }
  | path = path
    { Var path }
  | TYPE; stage = NUMERAL
    { Univ { stage } }
  | THE; tp = atomic_term; tm = atomic_term 
    { Ann { tm; tp} }
