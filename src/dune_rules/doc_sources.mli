open Import

type mld =
  { path : Path.Build.t (** The path to the mld/asset file *)
  ; in_doc : Path.Local.t (** Where in the doc hierarchy should be the mld/asset file *)
  }

(** Builds a map of [mld]s from the [(documentation ...)] stanza, compiling and
    merging entries from [(mld_files ...)] and [(files ...)] *)
val build_mlds_map
  :  Dune_file.t
  -> dir:Path.Build.t
  -> files:Filename.Set.t
  -> Expander.t
  -> (Documentation.t * mld list) list Memo.t
