module Basic = Basic
module Basic_display : Display.Type
module Color = Color
module Color_display : Display.Type
module Json = Json
module Json_display : Display.Type

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
