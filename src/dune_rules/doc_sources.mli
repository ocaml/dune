open Import

type mld =
  { path : Path.Build.t
  ; in_doc : Path.Local.t
  }

val build_mlds_map
  :  Dune_file.t
  -> dir:Stdune.Path.Build.t
  -> files:Import.String.Set.t
  -> Expander.t
  -> (Documentation.t * mld list) list Memo.t
