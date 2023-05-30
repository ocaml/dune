open Import

(** [files path] returns all the source dependencies starting from [path]. If
    [path] isn't in the source, the empty dependency set and an empty path set
    is returned *)
val files : Path.t -> (Dep.Set.t * Path.Set.t) Memo.t
