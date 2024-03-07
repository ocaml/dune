(** Add a [flush stderr] after every UI operation *)
val flush : Backend_intf.t -> Backend_intf.t

(** Creates a backend that writes to both backends in sequence *)
val compose : Backend_intf.t -> Backend_intf.t -> Backend_intf.t
