open Bwd

type severity =
  | Info
  | Warning
  | Error

(* [TODO: Reed M, 02/05/2022] Integrate this into the diagnostic system *)
type note = 
  | SourceNote of {source_code : string;
                   row : int;
                   start_col : int;
                   end_col : int;
                   note : string }
  | HelpNote of string

type cause =
  { filename : string;
    row : int;
    column : int;
    notes : note bwd }

type t =
  { code : string;
    severity : severity;
    message : string;
    cause : cause option; }

exception Fatal of t

let cause ~filename ~row ~column =
  { filename; row; column; notes = Emp }


let note ~source_code ?row ?(start_col = 0) ?end_col ~note cause =
  let end_col = Option.value end_col ~default:(String.length source_code) in
  let row = Option.value row ~default:cause.row in
  { cause with notes = Snoc(cause.notes, SourceNote { source_code; row; start_col; end_col; note }) }

let help str cause =
  { cause with notes = Snoc(cause.notes, HelpNote str) }

let info ?cause ~code message =
  { code; severity = Info; message; cause }

let warning ?cause ~code message =
  { code; severity = Warning; message; cause }

let error ?cause ~code message =
  { code; severity = Error; message; cause }

let pp_severity fmt =
  function
  | Info -> Format.fprintf fmt "Info"
  | Warning -> Format.fprintf fmt "Warning"
  | Error -> Format.fprintf fmt "Error"

let pp_note fmt  =
  function
  | SourceNote {source_code; row; start_col; end_col; note} ->
    let row_digits = 1 + (int_of_float @@ Float.log10 (float_of_int row)) in
    (* NOTE: The formatting here is complicated enough that it warrants
       using the Format.pp_print family of functions *)
    Format.pp_print_string fmt (String.make row_digits ' ');
    Format.pp_print_string fmt " |";
    Format.pp_print_newline fmt ();

    Format.pp_print_int fmt row;
    Format.pp_print_string fmt " |     ";
    Format.pp_print_string fmt source_code;
    Format.pp_print_newline fmt ();

    Format.pp_print_string fmt (String.make row_digits ' ');
    Format.pp_print_string fmt " |     ";
    Format.pp_print_string fmt (String.make start_col ' ');
    Format.pp_print_string fmt (String.make (end_col - start_col) '^');
    Format.pp_print_char fmt ' ';
    Format.pp_print_string fmt note
  | HelpNote help -> 
    (* [TODO: Reed M, 02/05/2022] When we have the nicer alignment, update this *)
    Format.fprintf fmt "Help:@. @[<h 2>@ %s]" help

let pp_cause fmt {filename; row; column; notes} =
  Format.fprintf fmt "@[<h 2>--> %s:%d:%d@]@.@[<h>%a@]"  
    filename
    row
    column
    (* [TODO: Reed M, 02/05/2022] Better separation for multiple notes *)
    (* [TODO: Reed M, 02/05/2022] Better Alignment for multiple notes *)
    (Format.pp_print_list pp_note) (BwdLabels.to_list notes)

let pp fmt {code; severity; message; cause} =
  Format.fprintf fmt "@[<h>%a[%s]: %s@]@,%a@."
    pp_severity severity
    code
    message
    (Format.pp_print_option pp_cause) cause
