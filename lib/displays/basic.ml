open Todo

let item_to_name f i = f (name i)

(* order items by category, priority, then due date *)
let order_items m =
  let open List in
  let l = item_list m in
  let cat_sort =
    sort (fun (_, i1) (_, i2) ->
      String.compare (item_to_name category i1) (item_to_name category i2)) l
  in
  let prio_sort =
    sort (fun (_, i1) (_, i2) ->
      Int.compare (item_to_name priority i2) (item_to_name priority i1)) cat_sort
  in
  sort (fun (_, i1) (_, i2) ->
    compare (item_to_name due_date i1) (item_to_name due_date i2)) prio_sort

let string_of_item i =
  let open Core in
  let open String in
  let n = name i in
  let len = length n in
  String.concat
    ~sep:"\n"
    [ n;
      make len '-';
      "Due:      " ^ due_date n;
      "Time:     " ^ time n;
      "Category: " ^ category n;
      "Urgency:  " ^ string_of_int (priority n);
      "Notes:    " ^ notes n;
      "Tags:     " ^ String.concat ~sep:", " (item_tags n);
      "Created:  " ^ created n;
      "Duration: " ^ string_of_int (duration n);
      "Complete: " ^ string_of_bool (complete n) ]

let display i = string_of_item i |> Printf.printf "%s\n"

let display_bindings m =
  let open Core in
  order_items m
  |> List.iter ~f:(fun (_, i) ->
          display i ;
          Out_channel.newline stdout)

let display_item name =
  match find_opt name !todo with
  | Some itm -> display itm
  | None -> Printf.printf "Item %s was not found in todo list\n" name

let due_today () =
  let open Core in
  let open Date in
  let today' = today ~zone:(Timezone.of_utc_offset ~hours:(-7)) in
  let today_items =
    filter
      (fun _ i ->
        String.equal (due_date (name i)) (Core.Date.to_string today'))
      !todo
  in
  display_bindings today_items

let active () =
  let open Core in
  let active_map = filter (fun _ i -> not (complete (name i))) !todo in
  display_bindings active_map

let priority_level n =
  let prio_level_map = filter (fun _ i -> priority (name i) = n) !todo in
  display_bindings prio_level_map

let completed () =
  let complete_map = filter (fun _ i -> complete (name i)) !todo in
  display_bindings complete_map

let category cat =
  let cat_map = filter (fun _ i -> String.equal (category (name i)) cat) !todo in
  display_bindings cat_map

let range date1 date2 =
  let item_ranges =
    filter
      (fun _ i ->
        let d = due_date (name i) in
        date1 <= d && d <= date2)
      !todo
  in
  display_bindings item_ranges

let duration_range ?min max =
  match min with
  | None ->
      let duration_map =
        filter
          (fun _ i ->
            match duration (name i) with
            (* 0 is default value *)
            | 0 -> false
            | dur -> dur > 0 && dur <= max)
          !todo
      in
      display_bindings duration_map
  | Some min ->
      let duration_map =
        filter
          (fun _ i ->
            match duration (name i) with
            | 0 -> false
            | dur -> dur > 0 && dur >= min && dur <= max)
          !todo
      in
      display_bindings duration_map

(* basic display, entire collection *)
let display_all () = display_bindings !todo
