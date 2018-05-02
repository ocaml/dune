open Stdune

val digest_db : Path.t

val db : Path.t

val log : Path.t

val to_delete_in_source_tree : Path.t

val to_promote : Path.t

val aliases : Path.t

(** Where we store stamp files for [stamp_file_for_files_of] *)
val misc : Path.t

(** File for the [(universe)] dependency. *)
val universe_file : Path.t

val install_dir : Path.t

val sandbox_dir : Path.t

