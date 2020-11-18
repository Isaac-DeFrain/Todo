open Todo

let item_to_name f i = f (name i)

let string_list_to_json = List.map (fun s -> `String s)

let item_to_json i =
  let n = name i in
  `Assoc
    [ (n,
        `Assoc
        [ ("Due date", `String (due_date n));
          ("Category", `String (category n));
          ("Priority", `Int (priority n));
          ("Time", `String (time n));
          ("Notes", `String (notes n));
          ("Tags", `List (string_list_to_json (item_tags n)));
          ("Duration", `Int (duration n));
          ("Created", `String (created n));
          ("Complete", `Bool (complete n))
        ]
      )
    ]

let display i = item_to_json i |> Yojson.pretty_to_string |> print_endline

let display_item name =
  try
    let itm = find name !todo in
    display itm
  with Not_found ->
    Printf.printf "Item %s was not found in todo list\n" name

let display_all () =
  let open List in
  let b = item_list !todo in
  let p_sort =
    sort (fun (_, i1) (_, i2) ->
      Int.compare (item_to_name priority i1) (item_to_name priority i2)) b
  in
  let sorted =
    sort (fun (_, i1) (_, i2) ->
      compare (item_to_name due_date i1) (item_to_name due_date i2)) p_sort
  in
  iter
    (fun (_, i) ->
      display i ;
      Core.Out_channel.newline stdout)
    sorted

(* TODO *)
let range _ _ = print_endline "Not implemented yet"

let due_today _ = print_endline "Not implemented yet"

let active _ = print_endline "Not implemented yet"

let priority_level _ = print_endline "Not implemented yet"

let completed _ = print_endline "Not implemented yet"

let category _ = print_endline "Not implemented yet"

let duration_range ?min max =
  if Option.is_none min && max = 0 then print_endline "Not implemented yet"
  else print_endline "Not implemented yet"
