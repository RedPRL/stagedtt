open Bwd

include Data.Domain
module S = Syntax

let local lvl =
  Neu { hd = Local lvl; spine = [] }

let global nm v =
  Neu { hd = Global (nm, v); spine = [] }


let push_frm (neu : neu) (frm : frm) ~(unfold : t -> t) : neu =
  match neu.hd with
  | Global (nm, v) ->
    { hd = Global (nm, Lazy.map unfold v); spine = frm :: neu.spine }
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
  | Struct _ -> Prec.juxtaposition
  | Quote _ -> Prec.juxtaposition
  | Neu _ -> Prec.atom
  | Code _ -> Prec.juxtaposition

let rec pp env =
  Pp.parens classify_tm env @@ fun fmt ->
  function
  | Lam (x, clo) ->
    let x, env = Pp.bind_var x env in
    Format.fprintf fmt "λ %s → %a"
      x
      (pp_clo S.pp env) clo
  | Struct _ -> failwith "[FIXME] pp: We are going to rework structs"

  | Quote v ->
    Format.fprintf fmt "↑[ %a ]"
      (pp env) v
  | Neu neu ->
    pp_neu env fmt neu
  | Code code -> pp_code env fmt code

and pp_clo pp_a env fmt =
  fun (Clo (a, clo_env)) ->
  match Env.locals clo_env with
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
        let x, envx = Pp.bind_var "v" env in
        Format.fprintf fmt "%s = %a; " x (pp env) (Lazy.force v);
        envx in
    Format.pp_print_string fmt "[ ";
    let env = pp_locals env fmt locals in
    let x, envx = Pp.bind_var "v" env in
    Format.fprintf fmt "%s = %a ⊢ %a ]"
      x
      (pp env) (Lazy.force v)
      (pp_a envx) a

and pp_neu env fmt {hd; spine} =
  Format.fprintf fmt "[ %a :: %a ]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "::") (pp_frm env)) spine
    (pp_hd env) hd

and pp_hd env fmt  =
  function
  | Local lvl ->
    Pp.lvl env fmt lvl
  | Global (nm, _) ->
    let x, _ = Pp.bind_var nm env in
    Format.pp_print_string fmt x

and pp_frm env fmt =
  function
  | Ap v ->
    Format.fprintf fmt "ap %a"
      (pp env) v
  | Proj lbl ->
    Format.fprintf fmt "proj %s"
      lbl
  | Splice ->
    Format.pp_print_string fmt "splice"

and pp_code env fmt =
  function
  | CodePi (base, fam) ->
    Format.fprintf fmt "Π %a %a"
      (pp env) base
      (pp env) fam
  | CodeSign _ -> failwith "[FIXME] pp_code: Going to rework signatures"
  | CodeUniv stage ->
    Format.fprintf fmt "type %d" stage
