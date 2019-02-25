(** Waiting for I/O readiness, process termination and signals *)

type event =
  | Ev_signal of int
  | Ev_child_process of int
  | Ev_read_fd of Unix.file_descr
  | Ev_write_fd of Unix.file_descr
  | Ev_urgent_fd of Unix.file_descr

val select : ?timeout:float -> event list -> event list

val enable_signal_event : int -> unit
val disable_signal_event : int -> unit

val acknowledge_signal : int -> unit
val acknowledge_termination : int -> Unix.process_status


