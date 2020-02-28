open Stdune

type is_component_of_a_group_but_not_the_root =
  { group_root : Path.Build.t
  ; stanzas : Stanza.t list Dir_with_dune.t option
  }

type t =
  | Generated
  | Source_only of File_tree.Dir.t
  | Standalone of File_tree.Dir.t * Stanza.t list Dir_with_dune.t
  (* Directory not part of a multi-directory group. The argument is [None] for
     directory that are not from the source tree, such as generated ones. *)
  | Group_root of
      File_tree.Dir.t
      * (Loc.t * Dune_file.Include_subdirs.qualification)
      * Stanza.t list Dir_with_dune.t
  (* Directory with [(include_subdirs x)] where [x] is not [no] *)
  | Is_component_of_a_group_but_not_the_root of
      is_component_of_a_group_but_not_the_root

(* Sub-directory of a [Group_root _] *)

module DB : sig
  type t

  type status

  val make :
    stanzas_per_dir:Dune_file.Stanzas.t Dir_with_dune.t Path.Build.Map.t -> t

  val get : t -> dir:Path.Build.t -> status
end
with type status := t
