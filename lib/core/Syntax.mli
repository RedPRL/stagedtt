open Data

include module type of Syntax

val app : t -> t -> t
val apps : t -> t list -> t

(** {1 Pretty Printing} *)
val pp : t Pp.printer

(** {1 Debug Printing} *)
val dump : Format.formatter -> t -> unit
