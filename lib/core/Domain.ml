open Prelude
module D = Data

module S = Syntax

type env = D.value_env =
  { locals : (t Lazy.t) bwd;
    size : int;
    tp_locals : tp bwd;
    tp_size : int
  }

and 'a clo = 'a D.vclo = 
  | Clo of 'a * env
and tm_clo = D.syntax D.vclo
and tp_clo = D.syntax_tp D.vclo

and t = D.value =
  | Lam of Ident.t * tm_clo
  | Quote of D.value
  | Neu of D.neu
  | Code of code

and tp = D.value_tp =
  | Pi of tp * Ident.t * tp_clo
  | Expr of tp
  | El of code
  | ElNeu of neu
  | Univ of int

and code = D.code = 
  | CodePi of t * t
  | CodeUniv of int

and neu = D.neu = { hd : hd; spine : frm list } 

and hd = D.hd = 
  | Local of int
  | Global of global
  | Hole of string option

and global =
  [ `Unstaged of Ident.path * D.value Lazy.t * D.inner Lazy.t ]

and frm = D.frm =
  | Ap of t
  | Splice

let local lvl =
  Neu { hd = Local lvl; spine = [] }

let global nm v inner =
  Neu { hd = Global (`Unstaged (nm, v, inner)); spine = [] }

let hole nm =
  Neu { hd = Hole nm; spine = [] }

let push_frm (neu : neu) (frm : frm) ~(unfold : t -> t) ~(stage : D.inner -> D.inner) : neu =
  match neu.hd with
  | Global (`Unstaged (nm, v, inner)) ->
    { hd = Global (`Unstaged (nm, Lazy.map unfold v, Lazy.map stage inner)); spine = frm :: neu.spine }
  | _ ->
    { hd = neu.hd; spine = frm :: neu.spine }

(** {1 Ugly Printing} *)
module Prec =
struct
  include Pp.Prec

  let passed = nonassoc 8
  let atom = nonassoc 7
  let delimited = nonassoc 6
  let juxtaposition = left 5
  let proj = right 4
  let arrow = right 3
  (* [TODO: Reed M, 02/05/2022] Figure out these *)
  let quote = right 3
  let splice = right 3
  let colon = nonassoc 2
  let arrow = right 1
  let in_ = nonassoc 0
end

let classify_tm =
  function
  | Lam _ -> Prec.arrow
  | Quote _ -> Prec.juxtaposition
  | Neu _ -> Prec.atom
  | Code _ -> Prec.juxtaposition

let classify_tp =
  function
  | Pi _ -> Prec.arrow
  | Univ _ -> Prec.atom
  | Expr _ -> Prec.delimited
  | El _ -> Prec.passed
  | ElNeu _ -> Prec.passed

let pp_clo (pp_tm : t Pp.printer) (pp_a : 'a Pp.printer) env fmt : 'a clo -> unit =
  fun (Clo (a, clo_env)) ->
  match clo_env.locals with
  | Emp ->
    Format.fprintf fmt "[ ⊢ %a ]"
      (pp_a env) a
  | Snoc (locals, v) ->
    (* NOTE: We repeat ourselves a bit here to properly handle delimiters. *)
    let rec pp_locals env fmt =
      function
      | Emp -> env
      | Snoc (locals, v) ->
        let env = pp_locals env fmt locals in
        let x, envx = Pp.bind_var (Ident.user "v") env in
        Format.fprintf fmt "%s = %a; " x (pp_tm env) (Lazy.force v);
        envx in
    Format.pp_print_string fmt "[ ";
    let env = pp_locals env fmt locals in
    let x, envx = Pp.bind_var (Ident.user "v") env in
    Format.fprintf fmt "%s = %a ⊢ %a ]"
      x
      (pp_tm env) (Lazy.force v)
      (pp_a envx) a

let rec pp env =
  Pp.parens classify_tm env @@ fun fmt ->
  function
  | Lam (x, clo) ->
    let x, env = Pp.bind_var x env in
    Format.fprintf fmt "λ %s → %a"
      x
      (pp_clo pp S.pp env) clo

  | Quote v ->
    Format.fprintf fmt "↑[ %a ]"
      (pp env) v
  | Neu neu ->
    pp_neu env fmt neu
  | Code code -> pp_code env fmt code


and pp_neu env fmt {hd; spine} =
  Format.fprintf fmt "[ %a :: %a ]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "::") (pp_frm env)) spine
    (pp_hd env) hd

and pp_hd env fmt  =
  function
  | Local lvl ->
    Pp.lvl env fmt lvl
  | Global (`Unstaged (path, _, _)) ->
    let x, _ = Pp.bind_var (User path) env in
    Format.pp_print_string fmt x
  | Hole nm ->
    Format.fprintf fmt "?%a"
      (Format.pp_print_option Format.pp_print_string) nm

and pp_frm env fmt =
  function
  | Ap v ->
    Format.fprintf fmt "ap %a"
      (pp env) v
  | Splice ->
    Format.pp_print_string fmt "splice"

and pp_code env fmt =
  function
  | CodePi (base, fam) ->
    Format.fprintf fmt "Π %a %a"
      (pp env) base
      (pp env) fam
  | CodeUniv stage ->
    Format.fprintf fmt "type %d" stage

let rec pp_tp env =
  Pp.parens classify_tp env @@ fun fmt ->
  function
  | Pi (base, ident, clo) ->
    let x, envx = Pp.bind_var ident env in
    Format.fprintf fmt "(%a : %s) → %a"
      (pp_tp (Pp.left_of Prec.colon env)) base
      x
      (pp_clo pp S.pp_tp (Pp.right_of Prec.arrow envx)) clo
  | Univ stage ->
    Format.fprintf fmt "type %d"
      stage
  | Expr tp ->
    Format.fprintf fmt "⇑[ %a ]"
      (pp_tp (Pp.isolated env)) tp
  | El tm ->
    pp_code env fmt tm
  | ElNeu neu ->
    pp_neu env fmt neu

module Env =
struct
  let empty =
    { locals = Emp;
      size = 0;
      tp_locals = Emp;
      tp_size = 0 }

  let from_vals locals size =
    { locals;
      size;
      tp_locals = Emp;
      tp_size = 0 }

  let lookup_idx env idx =
    Bwd.nth_opt env.locals idx

  let lookup_tp_idx env idx =
    BwdLabels.nth_opt env.tp_locals idx

  let locals env =
    env.locals

  let size env =
    env.size

  let tp_size env =
    env.tp_size

  let extend env v =
    { env with
      locals = Snoc(env.locals, Lazy.from_val v);
      size = env.size + 1 }

  let extend_tp env tp =
    { env with
      tp_locals = Snoc(env.tp_locals, tp);
      tp_size = env.tp_size + 1 }
end
