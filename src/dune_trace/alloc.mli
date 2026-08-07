type t

type snapshot =
  { config : Event.alloc_config
  ; exact : Event.alloc_exact
  ; minor : Event.alloc_heap
  ; major : Event.alloc_heap
  ; promoted : Event.alloc_heap
  }

val start : unit -> t
val reset : t -> unit
val stop : t -> unit
val snapshot : t -> snapshot
