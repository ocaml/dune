open Import

type is_component_of_a_group_but_not_the_root =
  { group_root : Path.Build.t
  ; stanzas : Dune_file.t option
  }

type t =
  | Generated
  | Source_only of Source_tree.Dir.t
  | Standalone of Source_tree.Dir.t * Dune_file.t
  (* Directory not part of a multi-directory group. The argument is [None] for
     directory that are not from the source tree, such as generated ones. *)
  | Group_root of
      Source_tree.Dir.t
      * (Loc.t * Dune_file.Include_subdirs.qualification)
      * Dune_file.t
  (* Directory with [(include_subdirs x)] where [x] is not [no] *)
  | Is_component_of_a_group_but_not_the_root of
      is_component_of_a_group_but_not_the_root

(* Sub-directory of a [Group_root _] *)

module DB : sig
  val get : dir:Path.Build.t -> t Memo.t
end
