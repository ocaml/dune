(** Wait for the filesystem clock to advance rather than dropping cached digest
    entries *)
val wait_for_filesystem_clock : bool ref

(** Print debug info for cached digests *)
val debug_digests : bool ref
