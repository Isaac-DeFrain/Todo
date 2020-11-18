(* Tags implementation *)
module TagMap = Map.Make (String)

type tag = int TagMap.t

let tags = ref TagMap.empty

let add_tags tags' =
  let new_tags =
    List.fold_right
      (fun t m ->
        match TagMap.find_opt t m with
        | None -> TagMap.add t 1 m
        | Some n -> TagMap.add t (n + 1) m)
      tags'
      !tags
  in
  tags := new_tags

let display_tags () = TagMap.iter (fun t _ -> Printf.printf "%s\n" t) !tags

let all_tags () =
  let bindings = TagMap.bindings !tags in
  List.map fst bindings
