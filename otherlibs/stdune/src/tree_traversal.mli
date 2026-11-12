(** [traverse ~src ~dst ... ()] calls [on_file] for regular files and [mkdir] for
    directories, preserving relative paths under [dst].

    Unlike [Fpath.traverse], it visits the root too, accepts a file root, and
    passes source/destination [Path.t] pairs. Callbacks return [unit]; there is no
    accumulator, pruning, sorting or custom traversal-error handling.

    [mkdir] runs before visiting contents. For a file root, the caller must
    arrange for [dst]'s parent to exist. [on_symlink] defaults to [`Resolve] for
    descendants; root symlinks are always followed. *)
val traverse
  :  src:Path.t
  -> dst:Path.t
  -> on_file:(src:Path.t -> dst:Path.t -> unit)
  -> mkdir:(src:Path.t -> dst:Path.t -> unit)
  -> on_unsupported:(src:Path.t -> File_kind.t -> unit)
  -> ?on_symlink:
       [ `Resolve | `Ignore | `Raise | `Call of src:Path.t -> Unix.file_kind option ]
  -> unit
  -> unit
