(** Generate a META file *)

open Import

val requires : ?preds:Meta.predicate list -> Lib_name.Set.t -> Meta.entry

(** Generate the meta for a package containing some libraries *)
val gen :
     package:Package.t
  -> add_directory_entry:bool
  -> Scope.DB.Lib_entry.t list
  -> Meta.t Memo.t
