open Import

(** Generate a META file *)

val requires : ?preds:Meta.predicate list -> Lib_name.Set.t -> Meta.entry

type rocq_theory =
  { public_name : Lib_name.t
  ; rocqpath : string
  ; deps : Lib_name.Set.t
  ; synopsis : string option
  }

(** Generate the meta for a package containing some libraries *)
val gen
  :  ?rocq_theories:rocq_theory list
  -> package:Package.t
  -> add_directory_entry:bool
  -> Scope.DB.Lib_entry.t list
  -> Meta.t Action_builder.t
