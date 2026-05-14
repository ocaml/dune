type t

type snapshot =
  { minor : Event.alloc_heap
  ; major : Event.alloc_heap
  ; promoted : Event.alloc_heap
  }

val start : unit -> t
val reset : t -> unit
val stop : t -> unit
val snapshot : t -> snapshot
