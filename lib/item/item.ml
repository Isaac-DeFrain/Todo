type date = Core.Date.t
type time = Core.Time.t

type item =
  { mutable name : string;
    mutable due_date : date option;
    mutable category : string;
    mutable priority : int;
    mutable complete : bool;
    mutable duration : int;
    mutable time : time option;
    mutable notes : string option;
    mutable tags : string list;
    created : time
  }
