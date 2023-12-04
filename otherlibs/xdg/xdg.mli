(** Base directories. Values of type {!t} are created using {!create}. *)
type t

(** The user's home directory. Uses [$USERPROFILE] on Windows, [$HOME]
    otherwise. *)
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

(** Constructor of type {!t}. [~win32] (default: {!Sys.win32}) determines
    whether to use Win32-specific APIs. [~env] is the function to get
    environment variables, typically {!Sys.getenv_opt}. *)
val create : ?win32:bool -> env:(string -> string option) -> unit -> t
