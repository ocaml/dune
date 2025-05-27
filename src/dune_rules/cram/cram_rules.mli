(** Rules for setting up cram tests *)

open Import

(** The type of errors that can occur when searching for cram tests *)
type error = Missing_run_t of Cram_test.t

(** [missing_run_t cram_test] raises an error message explaining that the
    directory test [cram_test] is missing it's [run.t]. *)
val missing_run_t : Cram_test.t -> 'a

(** Memoized list of cram tests in a directory. *)
val cram_tests : Source_tree.Dir.t -> (Cram_test.t, error) result list Memo.t

(** Cram test rules *)
val rules : sctx:Super_context.t -> dir:Path.Build.t -> Source_tree.Dir.t -> unit Memo.t
