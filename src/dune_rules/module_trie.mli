(** This module defines a prefix tree that stores a map of module names at every
    non-leaf node. Most notably, it's used to implement `(include_subdirs
    qualified)` where the directory name qualifies the namespace for its
    descendant modules in the file system. *)

open Import

include
  Module_trie_intf.S with type 'a map := 'a Module_name.Map.t and type el := Module_name.t

module By_dir :
  Module_trie_intf.S with type 'a map := 'a Filename.Map.t and type el := Filename.t
