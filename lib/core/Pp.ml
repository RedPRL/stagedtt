open Bwd
module StringMap = Map.Make (String)

(* NOTE: These are somewhat easy to get wrong, so we wrap them
   in an additional module signature. *)
module Prec : sig
  type t
  val nonassoc : int -> t
  val left : int -> t
  val right : int -> t
  val prefix : int -> t
  val postfix : int -> t

  type env
  val left_of : t -> env
  val right_of : t -> env
  val surrounded_by : t -> env
  val isolated : env
  val isolate_left : t -> env
  val isolate_right : t -> env

  val parens : env -> t -> bool
end =
struct
  type t = int * int

  let nonassoc n = 2*n, 2*n
  let left n = 2*n, 2*n+1
  let right n = 2*n+1, 2*n
  let prefix n = Int.max_int, 2*n
  let postfix n = 2*n, Int.max_int

  type env = int * int

  let left_of (l, _) = Int.min_int, l
  let right_of (_, r) = r, Int.min_int
  let surrounded_by (l, r) = r, l
  let isolated = Int.min_int, Int.min_int
  let isolate_left (_, r) = Int.min_int, r
  let isolate_right (l, _) = l, Int.min_int

  let parens (l', r') (l, r) = l' >= l || r' >= r
end

type env =
  { prec : Prec.env;
    (* [TODO: Reed M, 02/05/2022] Use a better datastructure. *)
    vars : string bwd;
    shadowed : int StringMap.t; }

let init =
  { prec = Prec.isolated;
    vars = Emp;
    shadowed = StringMap.empty }

type 'a printer = env -> Format.formatter -> 'a -> unit

let left_of prec env =
  { env with prec = Prec.left_of prec }

let right_of prec env =
  { env with prec = Prec.right_of prec }

let surrounded_by prec env =
  { env with prec = Prec.surrounded_by prec }

let isolated env =
  { env with prec = Prec.isolated }

let isolate_left prec env =
  { env with prec = Prec.isolate_left prec }

let isolate_right prec env =
  { env with prec = Prec.isolate_right prec }

let subscript =
  function
  | 0 -> "₀"
  | 1 -> "₁"
  | 2 -> "₂"
  | 3 -> "₃"
  | 4 -> "₄"
  | 5 -> "₅"
  | 6 -> "₆"
  | 7 -> "₇"
  | 8 -> "₈"
  | 9 -> "₉"
  | _ -> invalid_arg "subscript"

let rec suffix str =
  (* NOTE: The repeated appending of lists may seem inefficient,
     but recall that we only have to do O(log_10 n) appends, where
     n is the suffix.*)
  function
  | 0 -> str
  | n -> (suffix str (n / 10)) ^ (subscript (n mod 10))

let bind_var x env =
  (* [NOTE: Pretty Printing + Renaming Variables]
     In our benchmarking of cooltt, we've found that a /huge/ amount of time
     can be spent renaming variables inside of the pretty printer. Therefore,
     we take care to use a much faster algorithm here. However, this does come
     with a drawback: we can't use numeric unicode subscripts like '₁' in identifiers. *)
  match StringMap.find_opt x env.shadowed with
  | Some n -> suffix x n, { env with shadowed = StringMap.add x (n + 1) env.shadowed }
  | None -> x , { env with shadowed = StringMap.add x 1 env.shadowed }

let var env fmt ix =
  (* [TODO: Reed M, 03/05/2022] Conditional compilation *)
  match BwdLabels.nth_opt env.vars ix with
  | Some s -> Format.pp_print_string fmt s
  | _      -> failwith @@ Format.asprintf "Pp.env: index %d out of bounds" ix

let lvl env fmt lvl =
  Format.pp_print_string fmt @@ BwdLabels.nth env.vars (BwdLabels.length env.vars - lvl - 1)

let parens classify env pp fmt t =
  if Prec.parens env.prec (classify t) then
    Format.fprintf fmt "(%a)" pp t
  else
    pp fmt t
