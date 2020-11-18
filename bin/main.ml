open Todo
open Core
open Displays

(* let spec =
  Command.Spec.(
    empty
    +> flag "-m" ()
      ~doc:" Make new todo list item with given attributes"
    +> flag "-stat" ()
      ~doc:" Todo list statistics"
  ) *)

let add =
  Command.basic_spec
    ~summary:"Add items to the todo list"
    ~readme:(fun () ->
      String.concat
        ~sep:"\n"
        [ "Must provide a name for your todo list item. Optionally, you can \
           also provide:";
          " - category";
          " - duration";
          " - priority level";
          " - notes";
          " - due date (YYYY-MM-DD)";
          " - tags" ])
    Command.Spec.(
      empty +> anon ("name" %: string) (* +> anon ("due_date" %: int) *))
    (fun n () -> make (* cat dur prio notes dd time tags *) ~name:n ())

(* let update =
  Command.basic_spec
    ~summary:"Modify existing todo list items"
    Command.Spec.(
      empty
      +> anon ("name" %: string)
      +> anon ("date2" %: date)
    )
    (fun date1 date2 () ->
      Date.diff date1 date2
      |> printf "%d days\n"
    ) *)

let display =
  Command.basic_spec
    ~summary:"Supply a display mode to display the todo list"
    ~readme:(fun () ->
      "Currently there is support for 3 display MODEs:\n\
      \ - basic\n\
      \ - color\n\
      \ - json\n")
    Command.Spec.(empty +> anon ("mode" %: string))
    (fun mode () -> display_all ~mode)

let command =
  Command.group
    ~summary:"Interact with todo list"
    [("add", add); ("display", display)]

let () = Command.run ~version ~build_info:"WIP" command

(* TODO: interactive *)
