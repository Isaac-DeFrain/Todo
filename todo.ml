(* Todo Item Implementation *)
module Todo = Map.Make(String)
open Todo

type date = Core.Date.t
type time = Core.Time.t

type item =
  { mutable name : string
  ; mutable due_date : date option
  ; mutable category : string
  ; mutable priority : int
  ; mutable complete : bool
  ; mutable duration : int
  ; mutable time : time option
  ; mutable notes : string option
  ; mutable tags : string list
  ; created : time
  }

let cmp_date d1 d2 =
  match (d1, d2) with
  | (None, _) -> -1
  | (_, None) -> 1
  | (Some d1, Some d2) -> Core.Date.compare d1 d2

(* Todo list data structure *)
type todo = item Todo.t
let todo : todo ref = ref empty

(* data structure for tags *)
module TagMap = Map.Make(String)
type tag = int TagMap.t
let tag_map : tag ref = ref TagMap.empty

let display_tags () =
  TagMap.iter (fun t _ ->
    Printf.printf "%s\n" t
  ) !tag_map

let all_tags () =
  let bindings = TagMap.bindings !tag_map in
  List.map fst bindings

let size () = cardinal !todo

let version = "v0.0.1"

(* conversion functions *)
let string_of_time i =
  match i.time with
  | None -> ""
  | Some t -> Core.Time.to_string t

let time_of_string_opt =
  Option.map Core.Time.of_string

let string_of_notes i =
  match i.notes with
  | None -> ""
  | Some n -> n

let due_date_of_string_opt =
  Option.map Core.Date.of_string

let string_of_due_date i =
  match i.due_date with
  | None -> ""
  | Some dd -> Core.Date.to_string dd

let string_of_duration i =
  let dur = i.duration in
  if dur > 0 then string_of_int dur ^ " min"
  else ""

let string_of_tags i = String.concat ", " i.tags

let add_tags tags =
  let new_tags = List.fold_right (fun t m ->
    match TagMap.find_opt t m with
    | None -> TagMap.add t 1 m
    | Some n -> TagMap.add t (n+1) m
  ) tags !tag_map in
  tag_map := new_tags

(* user task creation function *)
let make ?(category="default") ?(duration=0) ?(priority=0)
         ?notes ?due_date ?time ?(tags=[]) ~name () =
  let now = Core.Time.now () in
  let due_date = due_date_of_string_opt due_date in
  let time = time_of_string_opt time in
  let tags = List.sort_uniq String.compare tags in
  let itm =
    { name; due_date; category; priority; notes; duration;
      complete=false; time; created=now ; tags } in
  (* update todo list with new task *)
  let new_todo = add name itm !todo in
  todo := new_todo;
  (* update used tags map *)
  add_tags tags

(* name access functions *)
let due_date name =
  try
    let itm = find name !todo in
    string_of_due_date itm
  with Not_found ->
  "Item " ^ name ^ " was not found in todo list"

let notes name =
  try
    let itm = find name !todo in
    match itm.notes with
    | None -> "No notes provided."
    | Some n -> n
  with Not_found ->
  "Item " ^ name ^ " was not found in todo list"

let category name =
  try
    let itm = find name !todo in
    itm.category
  with Not_found ->
  "Item " ^ name ^ " was not found in todo list"

let time name =
  try
    let itm = find name !todo in
    match itm.time with
    | None -> "No time recorded"
    | Some t -> Core.Time.to_string t
  with Not_found ->
  "Item " ^ name ^ " was not found in todo list"

let priority name =
  try
    let itm = find name !todo in
    itm.priority
  with Not_found ->
  Printf.printf "Item %s was not found in todo list\n" name;
  -1

let complete name =
  try
    let itm = find name !todo in
    itm.complete
  with Not_found ->
  Printf.printf "Item %s was not found in todo list\n" name;
  false

let created name =
  try
    let itm = find name !todo in
    Core.Time.to_string itm.created
  with Not_found ->
  "Item " ^ name ^ " was not found in todo list"

let duration name =
  try
    let itm = find name !todo in
    itm.duration
  with Not_found ->
  Printf.printf "Item %s was not found in todo list\n" name;
  -1

let tags name =
  try
    let itm = find name !todo in
    itm.tags
  with Not_found ->
  Printf.printf "Item %s was not found in todo list\n" name;
  []

(* single attribute update functions *)
let update_todo itm =
  let new_todo = add itm.name itm !todo in
    todo := new_todo; itm

let update_due due_date itm =
  match due_date with
  | None -> itm
  | dd ->
    itm.due_date <- Option.map Core.Date.of_string dd;
    update_todo itm

let update_notes notes itm =
  match notes with
  | None -> itm
  | n ->
    itm.notes <- n;
    update_todo itm

let update_category category itm =
  match category with
  | None -> itm
  | Some c ->
    itm.category <- c;
    update_todo itm

let update_time time itm =
  match time with
  | None -> itm
  | Some _ as t ->
    itm.time <- Option.map Core.Time.of_string t;
    update_todo itm

let update_urg priority itm =
  match priority with
  | None -> itm
  | Some u ->
    itm.priority <- u;
    update_todo itm

let update_complete complete itm =
  match complete with
  | None -> itm
  | Some c ->
    itm.complete <- c;
    update_todo itm

let update_duration duration itm =
  match duration with
  | None -> itm
  | Some d ->
    itm.duration <- d;
    update_todo itm

let update_tags tags itm =
  match tags with
  | None -> itm
  | Some t ->
    let cur_tags = itm.tags in
    let new_tags = List.sort_uniq String.compare (t @ cur_tags) in
    itm.tags <- new_tags;
    add_tags t;
    update_todo itm

(* update several attributes simultaneously *)
let update_more ?due_date ?notes ?category ?time ?duration
                ?priority ?complete ?tags ~name () =
  try
    let itm = find name !todo in
    update_due due_date itm
    |> update_notes notes
    |> update_category category
    |> update_time time
    |> update_urg priority
    |> update_duration duration
    |> update_complete complete
    |> update_tags tags
    |> ignore
  with Not_found ->
  Printf.printf "Item %s was not found in todo list\n" name

(* use first-class modules to choose different display formats *)
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

module Basic = struct

  (* order bindings by category, priority, then due date *)
  let order_bindings m =
    let open List in
    let l = bindings m in
    let cat_sort = sort (fun (_, i1) (_, i2) ->
      String.compare i1.category i2.category) l in
    let prio_sort = sort (fun (_, i1) (_, i2) ->
      Int.compare i2.priority i1.priority) cat_sort in
    sort (fun (_, i1) (_, i2) -> cmp_date i1.due_date i2.due_date) prio_sort

  let string_of_item i =
    let open Core in
    let open String in
    let n = i.name in
    let len = length n in
    String.concat ~sep:"\n"
      [ n
      ; make len '-'
      ; "Due:      " ^ string_of_due_date i
      ; "Time:     " ^ string_of_time i
      ; "Category: " ^ i.category
      ; "Urgency:  " ^ string_of_int i.priority
      ; "Notes:    " ^ string_of_notes i
      ; "Tags:     " ^ string_of_tags i
      ; "Created:  " ^ Time.to_string i.created
      ; "Duration: " ^ string_of_duration i
      ; "Complete: " ^ string_of_bool i.complete
      ]

  let display i = string_of_item i |> Printf.printf "%s\n"

  let display_bindings m =
    let open Core in
    order_bindings m
    |> List.iter ~f:(fun (_, i) -> display i; Out_channel.newline stdout)

  let display_item name =
    try
      let itm = find name !todo in
      display itm
    with Not_found ->
    Printf.printf "Item %s was not found in todo list\n" name

  let due_today () =
    let open Core in
    let open Date in
    let today = today ~zone:(Timezone.of_utc_offset ~hours:(-4)) in
    let today_map = filter (fun _ i ->
      match i.due_date with
      | None -> false
      | Some dd ->
        (=) dd today
    ) !todo
    in
    display_bindings today_map

  let active () =
    let open Core in
    let active_map = filter (fun _ i -> not i.complete) !todo in
    display_bindings active_map

  let priority_level n =
    let prio_level_map = filter (fun _ i -> i.priority = n) !todo in
    display_bindings prio_level_map

  let completed () =
    let complete_map = filter (fun _ i -> i.complete) !todo in
    display_bindings complete_map

  let category cat =
    let cat_map = filter (fun _ i -> String.equal i.category cat) !todo in
    display_bindings cat_map

  let range date1 date2 =
    let open Core in
    let open Date in
    let date1 = of_string date1 in
    let date2 = of_string date2 in
    let range_map = filter (fun _ i ->
      match i.due_date with
      | None -> false
      | Some dd ->
        (<=) date1 dd && (<=) dd date2
    ) !todo
    in
    display_bindings range_map

  let duration_range ?min max =
    match min with
    | None -> 
      let duration_map = filter (fun _ i ->
        match i.duration with
        (* 0 is default value *)
        | 0 -> false
        | dur -> dur > 0 && dur <= max
      ) !todo
      in
      display_bindings duration_map
    | Some min ->
      let duration_map = filter (fun _ i ->
        match i.duration with
        | 0 -> false
        | dur ->
          dur > 0 && dur >= min && dur <= max
      ) !todo
      in
      display_bindings duration_map

  (* basic display, entire collection *)
  let display_all () = display_bindings !todo
end

module Json = struct

  let string_list_to_json sl =
    List.map (fun s -> `String s) sl

  let item_to_json i =
    `Assoc
    [ (i.name, `Assoc
      [ ("Due date", `String (string_of_due_date i));
        ("Category", `String i.category);
        ("Priority", `Int    i.priority);
        ("Time"    , `String (string_of_time i));
        ("Notes"   , `String (string_of_notes i));
        ("Tags"    , `List   (string_list_to_json i.tags));
        ("Duration", `Int    i.duration);
        ("Created" , `String (Core.Time.to_string i.created));
        ("Complete", `Bool   i.complete)
      ])
    ]

  let display i =
    item_to_json i |> Yojson.pretty_to_string |> print_endline

  let display_item name =
    try
      let itm = find name !todo in
      display itm
    with Not_found ->
    Printf.printf "Item %s was not found in todo list\n" name
 
  let display_all () =
    let open List in
    let b = bindings !todo in
    let p_sort = sort (fun (_, i1) (_, i2) ->
      Int.compare i1.priority i2.priority) b in
    let sorted = sort (fun (_, i1) (_, i2) ->
      cmp_date i1.due_date i2.due_date) p_sort in
    iter (fun (_, i) -> (display i; Core.Out_channel.newline stdout)) sorted

  (* TODO *)
  let range        _ _ = print_endline "Not implemented yet"
  let due_today      _ = print_endline "Not implemented yet"
  let active         _ = print_endline "Not implemented yet"
  let priority_level _ = print_endline "Not implemented yet"
  let completed      _ = print_endline "Not implemented yet"
  let category       _ = print_endline "Not implemented yet"
  let duration_range ?min max =
    if Option.is_none min && max = 0 then
      print_endline "Not implemented yet"
    else print_endline "Not implemented yet"
end

(* color display functor *)
module Color (D : sig
  include Display
  val string_of_item : item -> string
  val order_bindings : item t -> (string * item) list
end) = struct

  let string_of_item = D.string_of_item
  let order_bindings = D.order_bindings

  let display i =
    string_of_item i
    |> Printf.printf "\027[38;5;%dm%s\027[0m\n" (Random.int 255)

  let display_item name =
    try
      let itm = find name !todo in
      display itm
    with Not_found ->
    Printf.printf
      "\027[38;5;%dmItem %s was not found in todo list\027[0m\n"
      (Random.int 255)
      name
 
  let display_all () =
    let open Core in
    let ordered = order_bindings !todo in
    List.iter ~f:(fun (_, i) -> (display i; Out_channel.newline stdout)) ordered

  (* TODO -- need: ... -> string list versions of these functions *)
  let range        _ _ = print_endline "Not implemented yet"
  let due_today      _ = print_endline "Not implemented yet"
  let active         _ = print_endline "Not implemented yet"
  let priority_level _ = print_endline "Not implemented yet"
  let completed      _ = print_endline "Not implemented yet"
  let category       _ = print_endline "Not implemented yet"
  let duration_range ?min max =
    if Option.is_none min && max = 0 then
      print_endline "Not implemented yet"
    else print_endline "Not implemented yet"
end

(* display modules *)
module Basic_display : Display = Basic
module Json_display  : Display = Json
module Color_display : Display = Color (Basic)

(* Display module map *)
module DisplayMap = Map.Make(String)
let display_map =
  DisplayMap.(
    empty
    |> add "basic" (module Basic_display : Display)
    |> add "json"  (module Json_display  : Display)
    |> add "color" (module Color_display : Display)
  )

(* module parameterized functions *)
let to_display use display_module =
  let name = String.lowercase_ascii display_module in
  match DisplayMap.find_opt name display_map with
  | None -> Printf.printf "Unknown display method: %s\n" display_module
  | Some m -> use m

let display_all ~mode:display_module =
  if is_empty !todo
  then Printf.printf "Todo list is currently empty\n"
  else to_display (fun m ->
    let module D = (val m) in D.display_all ()
  ) display_module

let display_due_today ~mode:display_module =
  to_display (fun m ->
    let module D = (val m) in D.due_today ()
  ) display_module

let display_active ~mode:display_module =
  to_display (fun m ->
    let module D = (val m) in D.active ()
  ) display_module

let display_priority_level ~mode:display_module n =
  to_display (fun m ->
    let module D = (val m) in D.priority_level n
  ) display_module

let display_completed ~mode:display_module =
  to_display (fun m ->
    let module D = (val m) in D.completed ()
  ) display_module

let display_category ~mode:display_module cat =
  to_display (fun m ->
    let module D = (val m) in D.category cat
  ) display_module

let display_item ~mode:display_module name =
  to_display (fun m ->
    let module D = (val m) in D.display_item name
  ) display_module

let display_range ~mode:display_module start stop =
  to_display (fun m ->
    let module D = (val m) in D.range start stop
  ) display_module

let display_duration_range ~mode:display_module ?min max =
  to_display (fun m ->
    let module D = (val m) in D.duration_range ?min max
  ) display_module

(* TODO: testing *)
(* more testing scenarios: tags, ... *)
module TestTodo = struct
  (* open Item.Todo *)
  let rand_cat () =
    let n = Random.int 4 in
    match n with
    | 0 -> "Default"
    | 1 -> "Work"
    | 2 -> "Personal"
    | 3 -> "Friends"
    | _ -> "Default"

  let test_tags =
    ["fun"; "inspiring"; "time-consuming"; "boring"; "painful"; "easy"]

  let rand_tags () =
    List.filter (fun _ -> Random.bool ()) test_tags

  let rand_dur () =
    let open Random in
    if bool () then int 120 else 0

  let rand_prio () = Random.int 3

  let _init_todo_test () =
    let open Core in
    let today = Date.today ~zone:(Timezone.of_utc_offset ~hours:(-5)) in
    let today = Date.to_string today in
    List.iter
      [ "Task01",today; "Task02",today; "Task03",today; "Task04","20201222"; "Task05","20200822"; "Task06","20200922"
      ; "Task07",today; "Task08",today; "Task09",today; "Task10","20210422"; "Task11","20201012"; "Task12","20201215"
      ]
      ~f:(fun (n, dd) ->
        make ~name:n ~priority:(rand_prio ()) ~category:(rand_cat ()) ~tags:(rand_tags ()) ~duration:(rand_dur ()) ~due_date:dd ());
    assert (size () = 12)

end

(* TODO: statistics *)
type statistics = 
  { mutable created : int
  ; mutable completed : int
  ; mutable histogram : int Map.Make(String).t
  ; mutable avg_time_hist : float Map.Make(String).t
  }
