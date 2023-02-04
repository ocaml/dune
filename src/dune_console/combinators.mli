(** Add a [flush stderr] after every UI operation *)
val flush : Backend_intf.t -> Backend_intf.t

(** Creates a backend that writes to both backends in sequence *)
val compose : Backend_intf.t -> Backend_intf.t -> Backend_intf.t

(** A backend that will signal [Sys.sigusr1] on an [EPIPE] error *)
val signal_usr1_on_pipe : Backend_intf.t -> Backend_intf.t
