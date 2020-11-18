(* TODO: statistics *)
type stats =
  { mutable created : int;
    mutable completed : int;
    mutable histogram : int Map.Make(String).t;
    mutable avg_time_hist : float Map.Make(String).t
  }
