(* TODO: more testing scenarios -- tags, ... *)

open Todo

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

let () =
  let open Core in
  let today = Date.today ~zone:(Timezone.of_utc_offset ~hours:(-5)) in
  let today = Date.to_string today in
  List.iter
    [ "Task01",today; "Task02",today; "Task03",today; "Task04","20201222"; "Task05","20200822"; "Task06","20200922"
    ; "Task07",today; "Task08",today; "Task09",today; "Task10","20210422"; "Task11","20201012"; "Task12","20201215"
    ]
    ~f:(fun (n, dd) ->
      make ~name:n ~priority:(rand_prio ()) ~category:(rand_cat ()) ~tags:(rand_tags ()) ~duration:(rand_dur ()) ~due_date:dd ());
  assert (size () = 12);
  print_endline "Correct initialization"
