(** Implement the XDG base directories specification

    http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html *)

type t

val home_dir : t -> string

(** The directory where the application should read/write config files. *)
val config_dir : t -> string

(** The directory where the application should read/write data files. *)
val data_dir : t -> string

(** The directory where the application should read/write cached files. *)
val cache_dir : t -> string

(** The directory where the application should read/write state files. *)
val state_dir : t -> string

(** The directory where the application should store socket files. *)
val runtime_dir : t -> string option

val create : ?win32:bool -> env:(string -> string option) -> unit -> t
