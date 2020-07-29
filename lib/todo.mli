(* Todo Item Interface *)

(** Types of todo items and attributes *)
type todo
type item

(** Version info *)
val version : string

(** Global todo list info *)
val size : unit -> int
val all_tags : unit -> string list

(** The only way to construct todo list items *)
val make :
    ?category:string -> ?duration:int -> ?priority:int ->
    ?notes:string -> ?due_date:string -> ?time:string ->
    ?tags:string list -> name:string -> unit -> unit

(** Extract info from item, given its name *)
val due_date  : string -> string
val duration  : string -> int
val notes     : string -> string
val category  : string -> string
val time      : string -> string
val priority  : string -> int
val created   : string -> string
val complete  : string -> bool
val tags      : string -> string list

(** The only way to modify todo list items *)
val update_more :
    ?due_date:string -> ?notes:string -> ?category:string ->
    ?time:string -> ?duration:int -> ?priority:int -> ?complete:bool ->
    ?tags:string list -> name:string -> unit -> unit

(** Display item options *)
module type Display = sig
  val due_today      : unit -> unit
  val active         : unit -> unit
  val priority_level : int  -> unit
  val completed      : unit -> unit
  val category       : string -> unit
  val display_all    : unit -> unit
  val display_item   : string -> unit
  val range          : string -> string -> unit
  val duration_range : ?min:int -> int -> unit
end

(** Different display options *)
module Json_display  : Display
module Basic_display : Display
module Color_display : Display

val display_due_today      : mode:string -> unit
val display_active         : mode:string -> unit
val display_priority_level : mode:string -> int -> unit
val display_completed      : mode:string -> unit
val display_category       : mode:string -> string -> unit
val display_all            : mode:string -> unit
val display_item           : mode:string -> string -> unit
val display_range          : mode:string -> string -> string -> unit
val display_duration_range : mode:string -> ?min:int -> int -> unit

val display_tags : unit -> unit

type statistics = 
    { mutable created : int
    ; mutable completed : int
    ; mutable histogram : int Map.Make(String).t
    ; mutable avg_time_hist : float Map.Make(String).t
    }
