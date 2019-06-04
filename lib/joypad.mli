type button = [ `A | `B | `Start | `Select | `Up | `Down | `Left | `Right ]

type t = {
  mutable control : Uint8.t;
  mutable arrows : Uint8.t;
  mutable buttons : Uint8.t;
}

val address : int

val write : t ->  Uint8.t -> unit
val make : unit -> t
val get : t -> Uint8.t
val button_down : t -> button -> unit
val button_up : t -> button -> unit
