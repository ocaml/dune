open Import

module Is_component_of_a_group_but_not_the_root : sig
  type t =
    { group_root : Path.Build.t
    ; stanzas : Dune_file.t option
    }
end

module Group_component : sig
  type t =
    { dir : Path.Build.t
    ; path_to_group_root : Filename.t list
    ; source_dir : Source_tree.Dir.t
    ; stanzas : Stanza.t list
    }
end

module Group_root : sig
  type t =
    { source_dir : Source_tree.Dir.t
    ; qualification : Loc.t * Include_subdirs.qualification
    ; dune_file : Dune_file.t
    ; components : Group_component.t list Memo.t
    }
end

type t =
  | Lock_dir
  | Generated
  | Source_only of Source_tree.Dir.t
  | Standalone of Source_tree.Dir.t * Dune_file.t
  (* Directory not part of a multi-directory group. The argument is [None] for
     directory that are not from the source tree, such as generated ones. *)
  | Group_root of Group_root.t
  (* Directory with [(include_subdirs x)] where [x] is not [no] *)
  | Is_component_of_a_group_but_not_the_root of Is_component_of_a_group_but_not_the_root.t

(* Sub-directory of a [Group_root _] *)

module DB : sig
  val get : dir:Path.Build.t -> t Memo.t
end

val directory_targets : t -> dir:Path.Build.t -> Loc.t Path.Build.Map.t Memo.t
