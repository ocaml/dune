(** Returns the time a file was created in the same time format as [Unix.stat].
    Returns a time if that information is available, none otherwise. Reasons *)
val stat : string -> float option

(** Identical to [stat] but will not follow symlinks, so this will return the
    creation time of the symlink *)
val lstat : string -> float option
