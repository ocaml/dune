(** IO operations. *)

val close_in : in_channel -> unit
val close_out : out_channel -> unit
val close_both : in_channel * out_channel -> unit
val input_lines : in_channel -> string list
val copy_channels : in_channel -> out_channel -> unit

include Io_intf.S with type path = Path.t
module String_path : Io_intf.S with type path = string

(** Symlink with fallback to copy on systems that don't support it. *)
val portable_symlink : src:Path.t -> dst:Path.t -> unit

(** Hardlink with fallback to copy on systems that don't support it. *)
val portable_hardlink : src:Path.t -> dst:Path.t -> unit

val set_copy_impl : [ `Portable | `Best ] -> unit
