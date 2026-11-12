open Import

type t =
  { source_filenames : Filename.Array.Set.t
  ; source_dirs : Filename.Array.Set.t
  ; rules : Rule.t list
  }

val select
  :  dir:Path.Build.t
  -> source_dir:Path.t
  -> build_dir_only_sub_dirs:Subdir_set.t
  -> source_filenames:Filename.Array.Set.t
  -> source_dirs:Filename.Array.Set.t
  -> Rule.t list
  -> t
