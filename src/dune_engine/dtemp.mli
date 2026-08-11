(** Temporary directories used by Dune and build actions. *)

open Import

(** This returns a build path, but we don't rely on that *)
val file : prefix:string -> suffix:string -> Path.t

(** Set the platform's temporary-directory environment variable. Build jobs use
    the action directory and internal jobs use Dune's directory. *)
val add_to_env : Env.t -> purpose:Process_metadata.purpose -> Env.t

(** Destroy the temporary file or directory *)
val destroy : Temp.what -> Path.t -> unit

(** Clear both temporary directories. *)
val clear : unit -> unit
