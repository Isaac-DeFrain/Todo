(* module Stats = Stats *)
module Tags = Tags

(* Todo item Implementation *)
module Todo = Map.Make (String)
open Todo

type item = Item.item

open Item

(* let cmp_date d1 d2 =
  match d1, d2 with
  | None, _ -> -1
  | _, None -> 1
  | Some d1, Some d2 -> Core.Date.compare d1 d2 *)

(* Todo list implementation *)
type todo = item Todo.t

let todo = ref empty

let item_list = bindings

let find = find

let find_opt = find_opt

let filter = filter

let is_empty = is_empty

let size () = cardinal !todo

let version = "v0.0.1"

let all_tags = Tags.all_tags

(* conversion functions *)
let name i = i.name

(* let string_of_time i =
  match i.time with None -> "" | Some t -> Core.Time.to_string t *)

let time_of_string_opt = Option.map Core.Time.of_string

(* let string_of_notes i = match i.notes with None -> "" | Some n -> n *)

let due_date_of_string_opt = Option.map Core.Date.of_string

let string_of_due_date i =
  match i.due_date with None -> "" | Some dd -> Core.Date.to_string dd

(* let string_of_duration i =
  let dur = i.duration in
  if dur > 0 then string_of_int dur ^ " min" else "" *)

(* let string_of_tags i = String.concat ", " i.tags *)

(* user task creation function *)
let make ?(category = "default") ?(duration = 0) ?(priority = 0) ?notes
    ?due_date ?time ?(tags = []) ~name () =
  let now = Core.Time.now () in
  let due_date = due_date_of_string_opt due_date in
  let time = time_of_string_opt time in
  let tags = List.sort_uniq String.compare tags in
  let itm =
    { name;
      due_date;
      category;
      priority;
      notes;
      duration;
      complete = false;
      time;
      created = now;
      tags
    }
  in
  (* update todo list with new task *)
  let new_todo = add name itm !todo in
  todo := new_todo ;
  (* update used tags map *)
  Tags.add_tags tags

(* name access functions *)
let not_found name = "Item " ^ name ^ " was not found in todo list"

let print_not_found = Printf.printf "%s\n"

let due_date name =
  Core.Option.value_map
    ~default:(not_found name)
    ~f:string_of_due_date
    (find_opt name !todo)

let notes name =
  let open Core in
  Option.value_map
    ~default:(not_found name)
    ~f:(fun itm ->
      Option.value_map ~default:"No notes recorded." ~f:Fun.id itm.notes)
    (find_opt name !todo)

let category name =
  Core.Option.value_map
    ~default:(not_found name)
    ~f:(fun itm -> itm.category)
    (find_opt name !todo)

let time name =
  let open Core in
  Option.value_map
    ~default:(not_found name)
    ~f:(fun itm ->
      match itm.time with
      | None -> "No time recorded"
      | Some t -> Core.Time.to_string t)
    (find_opt name !todo)

let priority name =
  Core.Option.value_map
    ~default:
      ( print_not_found name ;
        -1 )
    ~f:(fun itm -> itm.priority)
    (find_opt name !todo)

let complete name =
  try
    let itm = find name !todo in
    itm.complete
  with Not_found ->
    print_not_found name ;
    false

let created name =
  try
    let itm = find name !todo in
    Core.Time.to_string itm.created
  with Not_found -> not_found name

let duration name =
  try
    let itm = find name !todo in
    itm.duration
  with Not_found ->
    print_not_found name ;
    -1

let item_tags name =
  try
    let itm = find name !todo in
    itm.tags
  with Not_found ->
    print_not_found name ;
    []

(* single attribute update functions *)
let update_todo itm =
  let new_todo = add itm.name itm !todo in
  todo := new_todo ;
  itm

let update_due due_date itm =
  match due_date with
  | None -> itm
  | dd ->
      itm.due_date <- Option.map Core.Date.of_string dd ;
      update_todo itm

let update_notes notes itm =
  match notes with
  | None -> itm
  | n ->
      itm.notes <- n ;
      update_todo itm

let update_category category itm =
  match category with
  | None -> itm
  | Some c ->
      itm.category <- c ;
      update_todo itm

let update_time time itm =
  match time with
  | None -> itm
  | Some _ as t ->
      itm.time <- Option.map Core.Time.of_string t ;
      update_todo itm

let update_urg priority itm =
  match priority with
  | None -> itm
  | Some u ->
      itm.priority <- u ;
      update_todo itm

let update_complete complete itm =
  match complete with
  | None -> itm
  | Some c ->
      itm.complete <- c ;
      update_todo itm

let update_duration duration itm =
  match duration with
  | None -> itm
  | Some d ->
      itm.duration <- d ;
      update_todo itm

let update_tags tags itm =
  match tags with
  | None -> itm
  | Some t ->
      let cur_tags = itm.tags in
      let new_tags = List.sort_uniq String.compare (t @ cur_tags) in
      itm.tags <- new_tags ;
      Tags.add_tags t ;
      update_todo itm

(* update several attributes simultaneously *)
let update_more ?due_date ?notes ?category ?time ?duration ?priority ?complete
    ?tags ~name () =
  try
    let itm = find name !todo in
    update_due due_date itm |> update_notes notes |> update_category category
    |> update_time time |> update_urg priority |> update_duration duration
    |> update_complete complete |> update_tags tags |> ignore
  with Not_found -> print_not_found name
