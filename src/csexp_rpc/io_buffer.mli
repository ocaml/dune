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
