module type Type = sig
  val due_today : unit -> unit
  val active : unit -> unit
  val priority_level : int -> unit
  val completed : unit -> unit
  val category : string -> unit
  val display_all : unit -> unit
  val display_item : string -> unit
  val range : string -> string -> unit
  val duration_range : ?min:int -> int -> unit
end
