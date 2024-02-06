open Import

(** [nearest_vcs t fn] returns the version control system with the longest root
    path that is an ancestor of [fn]. *)
val nearest_vcs : Path.Source.t -> Vcs.t option Memo.t
