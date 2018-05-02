open Stdune

val digest_db : unit -> Path.t

val db : unit -> Path.t

val log : unit -> Path.t

val to_delete_source_tree : unit -> Path.t

val to_promote : unit -> Path.t

val aliases : unit -> Path.t

(** Where we store stamp files for [stamp_file_for_files_of] *)
val misc : unit -> Path.t

(** File for the [(universe)] dependency. *)
val universe_file : unit -> Path.t

val install_dir : unit -> Path.t

val sandbox_dir : unit -> Path.t
