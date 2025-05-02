open Import

(** This module serves as a wrapper around the C implementation of MD5 found in the OCaml
    compiler libraries. We use this implementation to avoid holding the GC lock whilst
    digesting. *)

(** MD5 Digests *)
type t

val to_dyn : t -> Dyn.t
val equal : t -> t -> bool

(** [Md5.file path] returns the [Md5.t] digest of the contents of the given file at
    [path]. *)
val file : Path.t -> t

(** [Md5.string s] returns the [Md5.t] digest of the string [s]. *)
val string : string -> t

(** [Md5.to_hex_string t] returns the printable hexadecimal representation of the given
    digest [t]. *)
val to_hex_string : t -> string

(** [Md5.of_hex_string s] convert a hexadecimal representation back into the corresponding
    digest. *)
val of_hex_string : string -> t option
