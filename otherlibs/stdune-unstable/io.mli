(** IO operations. *)

val close_in : in_channel -> unit

val close_out : out_channel -> unit

val close_both : in_channel * out_channel -> unit

val input_lines : in_channel -> string list

(** This function is not safe to use from multiple threads, even if operating on
    unrelated channels because it uses a statically-allocated global buffer. *)
val copy_channels : in_channel -> out_channel -> unit

val read_all : in_channel -> string

include Io_intf.S with type path = Path.t

module String_path : Io_intf.S with type path = string
