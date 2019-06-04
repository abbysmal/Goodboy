type t
type register
type flag

type paired_register = [ `Af | `Bc | `De | `Hl ] [@@deriving sexp]

val a : register
val b : register
val c : register
val d : register
val e : register
val h : register
val l : register

val z : flag
val n : flag
val hc : flag
val ca : flag

val sexp_of_register : register -> Sexplib.Sexp.t
val register_of_sexp : Sexplib.Sexp.t -> register

val sexp_of_flag : flag -> Sexplib.Sexp.t
val flag_of_sexp : Sexplib.Sexp.t -> flag
val make : unit -> t

val get : t -> register -> char
val set : t -> register -> char -> unit

val set_pair : t -> paired_register  -> int -> unit
val get_pair : t -> paired_register -> int

val is_flag_set : t -> flag -> bool
val set_flag : t -> flag -> unit
val unset_flag : t -> flag -> unit
val toggle_flag : t -> flag -> unit

val clear_flags : t -> unit
val write_flags : ?z:bool -> ?n:bool -> ?hc:bool -> ?ca:bool -> t -> unit
