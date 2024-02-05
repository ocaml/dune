(** IO operations. *)

val close_in : in_channel -> unit
val close_out : out_channel -> unit
val close_both : in_channel * out_channel -> unit
val input_lines : in_channel -> string list

(** This function is not safe to use from multiple threads, even if operating on
    unrelated channels because it uses a statically-allocated global buffer. *)
val copy_channels : in_channel -> out_channel -> unit

(** Try to read everything from a channel. Returns [Error ()] if the contents
    are larger than [Sys.max_string_length]. This is generally a problem only
    on 32-bit systems.
    Overflow detection does not happen in the following cases:
    - channel is not a file (for example, a pipe)
    - if the detected size is unreliable (/proc)
    - race condition with another process changing the size of the underlying
      file.
      In these cases, an exception might be raised by [Buffer] functions. *)
val read_all_unless_large : in_channel -> (string, unit) result

include Io_intf.S with type path = Path.t
module String_path : Io_intf.S with type path = string

(** Symlink with fallback to copy on systems that don't support it. *)
val portable_symlink : src:Path.t -> dst:Path.t -> unit

(** Hardlink with fallback to copy on systems that don't support it. *)
val portable_hardlink : src:Path.t -> dst:Path.t -> unit

val set_copy_impl : [ `Portable | `Best ] -> unit
