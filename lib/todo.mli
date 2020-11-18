(* Todo Item Interface *)

(** Types of todo items and attributes *)
type todo
type item = Item.item

val todo : todo ref

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

val name : item -> string

(** Extract info from item, given its name *)
val due_date  : string -> string
val duration  : string -> int
val notes     : string -> string
val category  : string -> string
val time      : string -> string
val priority  : string -> int
val created   : string -> string
val complete  : string -> bool
val item_tags : string -> string list

(** Todo data structure accessor functions *)
val item_list : todo -> (string * item) list
val find      : string -> todo -> item
val find_opt  : string -> todo -> item option

val filter    : (string -> item -> bool) -> todo -> todo
val is_empty  : todo -> bool

(** The only way to modify todo list items *)
val update_more :
    ?due_date:string -> ?notes:string -> ?category:string ->
    ?time:string -> ?duration:int -> ?priority:int -> ?complete:bool ->
    ?tags:string list -> name:string -> unit -> unit
