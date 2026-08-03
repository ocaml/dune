(** Storage for persistent workspace-local state. Values used by file-system
    memoization and the rule cache are serialized together so they can share
    structure. *)

open Import

val mark_dirty : unit -> unit

module Dir_contents : sig
  type t = File_kind.t Filename.Array.Map.t

  val repr : t Repr.t
end

module Fs_memo : sig
  module Stats : sig
    type t =
      { mtime : Time.t
      ; ctime : Time.t
      ; size : int
      ; perm : Unix.file_perm
      ; dev : int
      ; ino : int
      }

    val repr : t Repr.t
  end

  type 'a file =
    { mutable contents : 'a
    ; mutable stats : Stats.t
    ; mutable stats_checked : int
    }

  type t =
    { mutable checked_key : int
    ; mutable max_timestamp : Time.t
    ; table : Digest.t file Path.Table.t
    ; dir_contents : Dir_contents.t file Path.Table.t
    }
end

module Rule_cache : sig
  module Entry : sig
    type t =
      { rule_digest : Digest.t
      ; dynamic_deps_stages : (Dep.Set.t * Digest.t) list
      ; targets_digest : Digest.t
      }
  end

  type digest =
    { digest : Digest.t
    ; siblings : Digest.t Targets.Produced.t
    ; generation : int
    }

  (* [rules] is keyed by the first target of the rule. *)
  type t =
    { rules : Entry.t Path.Table.t
    ; digests : digest Path.Build.Table.t
    ; invalidated_subtrees : int Path.Build.Table.t
      (* A digest is only valid if its generation is greater or equal to the
         generation of all of its parents. *)
    ; mutable generation : int
    }
end

val file : Path.t
val fs_memo : unit -> Fs_memo.t
val rule_cache : unit -> Rule_cache.t
val loaded_from_disk : unit -> bool
val at_exit : At_exit.t
val load_fs_memo : unit -> Fs_memo.t option
