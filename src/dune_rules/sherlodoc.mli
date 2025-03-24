open Import

(** [search_db sctx ~dir odocls] is the path of the search database built by
    indexing the .odocl files [odocls]. A rule to built this database is added
    to [sctx].
    If sherlodoc is not installed, [None] is returned instead.*)
val search_db
  :  Super_context.t
  -> dir:Path.Build.t
  -> external_odocls:Path.Build.t list
  -> Path.Build.t list
  -> Path.Build.t Memo.t

val sherlodoc_dot_js : Super_context.t -> dir:Path.Build.t -> unit Memo.t

val odoc_args
  :  Super_context.t
  -> search_db:Path.Build.t
  -> dir_sherlodoc_dot_js:Path.Build.t
  -> _ Command.Args.t
