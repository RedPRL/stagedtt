%{
open Prelude
open Command
open CS

let locate (start, stop) node =
  {node; info = Some {start; stop}}

let ap_or_atomic =
  function
  | [] -> failwith "Impossible Internal Error"
  | [f] -> f.node
  | f :: args -> Ap (f, args)

%}

%token <int> NUMERAL
%token <string> ATOM
%token <string option> HOLE
%token <bool> FLAG
%token COLON COLON_COLON COLON_EQUALS RIGHT_ARROW UNDERSCORE
(* Symbols *)
%token LAMBDA
(* Delimiters *)
%token LPR RPR LSQ RSQ UP_LSQ DOWN_LSQ DOUBLE_UP_LSQ
(* Keywords *)
%token TYPE THE
(* Commands *)
%token DEF DEF_BANG NORMALIZE STAGE PRINT FAIL DEBUG QUIT
%token EOF

%start <Command.command list> commands
%type <CS.t>
  atomic_term
  term
%type <CS.t_>
  plain_arrow
  plain_atomic_term
  plain_term
%type <Ident.path>
  path
%type <Ident.t>
  name

%right RIGHT_ARROW

%%

%inline
located(X):
  | e = X
    { locate $loc e }

term: t = located(plain_term)
  { t }

atomic_term: t = located(plain_atomic_term)
  { t }

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
    { Def {ident; tp = Some tp; tm} }
  | DEF_BANG; ident = name; COLON; tp = term; COLON_EQUALS tm = term
    { DefStaged {ident; tp = Some tp; tm} }
  | FAIL; message = ATOM; tp = term; COLON_EQUALS tm = term
    { Fail {message; tp = Some tp; tm} }
  | NORMALIZE; tm = term; 
    { Normalize {tm} }
  | STAGE; path = path
    { Stage path }
  | PRINT; path = path;
    { Print path }
  | DEBUG; flag = FLAG;
    { Debug flag }
  | QUIT
    { Quit }

plain_term:
  | tms = nonempty_list(atomic_term)
    { ap_or_atomic tms }
  | tm = plain_arrow
    { tm }

plain_arrow:
  | LAMBDA; nms = list(name); RIGHT_ARROW; tm = term
    { Lam (nms, tm) }
  | LPR; ident = name; COLON; base = term; RPR; RIGHT_ARROW; fam = term
    { Pi (base, ident, fam) }
  | base = term; RIGHT_ARROW; fam = term
    { Pi (base, Anon, fam) }

plain_atomic_term:
  | LPR; tm = plain_term; RPR
    { tm }
  | DOUBLE_UP_LSQ; tm = term; RSQ
    { Expr tm }
  | UP_LSQ; tm = term; RSQ
    { Quote tm }
  | DOWN_LSQ; tm = term; RSQ
    { Splice tm }
  | path = path
    { Var path }
  | name = HOLE
    { Hole name }
  | TYPE; stage = NUMERAL
    { Univ { stage } }
  | THE; tp = atomic_term; tm = atomic_term 
    { Ann { tm; tp} }
