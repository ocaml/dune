open Import

(** [digest_with_lstat path] stats the [path] using [lstat] (without following
    symlinks) and computes a digest of its contents. Directories are allowed
    and will be digested recursively.

    Raises [User_error] if the path cannot be stat'd (e.g., does not exist or
    permission denied), cannot be digested (e.g., I/O error during reading), or
    has an unexpected file kind (e.g., socket, FIFO). *)
val digest_with_lstat : Path.t -> Dune_digest.t
