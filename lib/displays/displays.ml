module Basic = Basic
module Basic_display = Basic
module Color = Color
module Color_display = Color.Make(Basic)
module Json = Json
module Json_display = Json

open Todo
open Tags

(* Display module map *)
module DisplayMap = Map.Make (String)

let display_map =
  DisplayMap.(
    empty
    |> add "basic" (module Basic_display : Display.Type)
    |> add "json" (module Json_display : Display.Type)
    |> add "color" (module Color_display : Display.Type)
  )

(* module parameterized functions *)
let to_display use display_module =
  let name = String.lowercase_ascii display_module in
  match DisplayMap.find_opt name display_map with
  | None -> Printf.printf "Unknown display method: %s\n" display_module
  | Some m -> use m

let display_all ~mode:display_module =
  if is_empty !todo then Printf.printf "Todo list is currently empty\n"
  else
    to_display
      (fun m ->
        let module D = (val m) in
        D.display_all ())
      display_module

let display_due_today ~mode:display_module =
  to_display
    (fun m ->
      let module D = (val m) in
      D.due_today ())
    display_module

let display_active ~mode:display_module =
  to_display
    (fun m ->
      let module D = (val m) in
      D.active ())
    display_module

let display_priority_level ~mode:display_module n =
  to_display
    (fun m ->
      let module D = (val m) in
      D.priority_level n)
    display_module

let display_completed ~mode:display_module =
  to_display
    (fun m ->
      let module D = (val m) in
      D.completed ())
    display_module

let display_category ~mode:display_module cat =
  to_display
    (fun m ->
      let module D = (val m) in
      D.category cat)
    display_module

let display_item ~mode:display_module name =
  to_display
    (fun m ->
      let module D = (val m) in
      D.display_item name)
    display_module

let display_range ~mode:display_module start stop =
  to_display
    (fun m ->
      let module D = (val m) in
      D.range start stop)
    display_module

let display_duration_range ~mode:display_module ?min max =
  to_display
    (fun m ->
      let module D = (val m) in
      D.duration_range ?min max)
    display_module

let display_tags = display_tags
