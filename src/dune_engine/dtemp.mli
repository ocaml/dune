(** Temp directory used by dune processes *)

open Import

(** This returns a build path, but we don't rely on that *)
val file : prefix:string -> suffix:string -> Path.t

(** Add the temp env var to the environment passed or return the initial
    environment with the temp var added. *)
val add_to_env : Env.t -> Env.t

(** Destroy the temporary file or directory *)
val destroy : Temp.what -> Path.t -> unit
