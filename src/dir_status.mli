open Stdune

type t =
  | Standalone of
      (File_tree.Dir.t * Stanza.t list Dir_with_dune.t option) option
  (* Directory not part of a multi-directory group. The argument is
     [None] for directory that are not from the source tree, such as
     generated ones. *)

  | Group_root of File_tree.Dir.t
                  * Stanza.t list Dir_with_dune.t
  (* Directory with [(include_subdirs x)] where [x] is not [no] *)

  | Is_component_of_a_group_but_not_the_root of
      Stanza.t list Dir_with_dune.t option
  (* Sub-directory of a [Group_root _] *)

module DB : sig
  type t
  type status

  val make
    :  File_tree.t
    -> stanzas_per_dir:Dune_file.Stanzas.t Dir_with_dune.t Path.Map.t
    -> t

  val get : t -> dir:Path.t -> status

end with type status := t
