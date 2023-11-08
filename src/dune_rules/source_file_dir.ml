open Import

type t =
  { dir : Path.Build.t
  ; path_to_root : Filename.t list
  ; files : Filename.Set.t
  }
