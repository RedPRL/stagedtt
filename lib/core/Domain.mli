open Data

include module type of Domain

val local : int -> Domain.t
val global : string -> Domain.t Lazy.t -> Domain.t 
