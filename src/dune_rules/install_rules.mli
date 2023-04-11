open Import

val install_file :
  package:Package.Name.t -> findlib_toolchain:Context_name.t option -> string

val symlink_rules :
  Super_context.t -> dir:Path.Build.t -> (Subdir_set.t * Rules.t) Memo.t

(** Generate rules for [.dune-package], [META.<package-name>] files. and
    [<package-name>.install] files. *)
val gen_project_rules : Super_context.t -> Dune_project.t -> unit Memo.t

module Stanzas_to_entries : sig
  val stanzas_to_entries :
    Super_context.t -> Install.Entry.Sourced.t list Package.Name.Map.t Memo.t
end
