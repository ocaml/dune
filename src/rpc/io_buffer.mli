(** A resizable IO buffer *)

type t

val to_dyn : t -> Dyn.t

(** create a new io buffer *)
val create : size:int -> t

(** [read t n] reads [n] bytes *)
val read : t -> int -> unit

(** [write t csexps] write [csexps] to [t] while resizing [t] as necessary *)
val write_csexps : t -> Csexp.t list -> unit

(** a flush token is used to determine when a write has been completely flushed *)
type flush_token

(** [flush_token t] will be flushed whenever everything in [t] will be written *)
val flush_token : t -> flush_token

(** [flushed t token] will return [true] once all the data that was present in
    [t] when [token] was created will be written *)
val flushed : t -> flush_token -> bool

(** underlying raw buffer *)
val bytes : t -> Bytes.t

(** [pos t] in [bytes t] to read *)
val pos : t -> int

(** [length t] the number of bytes to read [bytes t] *)
val length : t -> int

(** [write_pos t] returns the write position inside the buffer we're allowed to
    write on *)
val write_pos : t -> int

(** [commit_write t ~len] tell the buffer [t] that we've written [len] writes.
    [len] must be smaller or equal to [max_write_len t] *)
val commit_write : t -> len:int -> unit

(** [max_write_len t] returns the maximum contiguous write size the buffer can
    fit *)
val max_write_len : t -> int

(** [read_char_exn t] reads and returns the next available byte. [t] must not be
    empty *)
val read_char_exn : t -> char

(** [read_into_buffer t buf ~max_len] reads at most [max_len] from [t] and
    appends what was read to [buf] *)
val read_into_buffer : t -> Buffer.t -> max_len:int -> int

(** [maybe_resize_to_fit t size] resizes [t] to fit a write of size [size] if
    necessary *)
val maybe_resize_to_fit : t -> int -> unit
