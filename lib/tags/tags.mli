(** Tags *)
type tag

val tags : tag ref
val all_tags : unit -> string list
val add_tags : string list -> unit
val display_tags : unit -> unit
