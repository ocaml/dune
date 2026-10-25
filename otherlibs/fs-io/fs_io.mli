(** Try to read everything from a channel. Returns [Error ()] if the contents
    are larger than [Sys.max_string_length]. This is generally a problem only
    on 32-bit systems.
    Overflow detection does not happen in the following cases:
    - channel is not a file (for example, a pipe)
    - if the detected size is unreliable (/proc)
    - race condition with another process changing the size of the underlying
      file.
      In these cases, an exception might be raised by [Buffer] functions. *)
val read_all_unless_large : in_channel -> (string, exn) result

val read_file : string -> (string, exn) result

(** Read through a file descriptor without creating a channel. Returns [None]
    if the file exceeds [max_size] or [Sys.max_string_length], or ends before
    its reported size. *)
val read_file_if_small : string -> max_size:int -> (string option, exn) result

val write_file : perm:int -> path:string -> data:string -> (unit, exn) result

(** Write the entire string without closing the descriptor. *)
val write_fd : Unix.file_descr -> string -> (unit, exn) result
