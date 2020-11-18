open Todo

module Make (D :
sig
  include Display.Type

  val string_of_item : item -> string

  val order_items : todo -> (string * item) list
end) =
struct
  let string_of_item = D.string_of_item

  let order_bindings = D.order_items
  
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
    List.iter
      ~f:(fun (_, i) ->
        display i ;
        Out_channel.newline stdout)
      ordered

  (* TODO -- need: ... -> string list versions of these functions *)
  let range _ _ = print_endline "Not implemented yet"

  let due_today _ = print_endline "Not implemented yet"

  let active _ = print_endline "Not implemented yet"

  let priority_level _ = print_endline "Not implemented yet"

  let completed _ = print_endline "Not implemented yet"

  let category _ = print_endline "Not implemented yet"

  let duration_range ?min max =
    if Option.is_none min && max = 0 then print_endline "Not implemented yet"
    else print_endline "Not implemented yet"
end
