open! Stdune

type rename_and_edit =
  { original_file : Path.Source.t
  ; extra_files_to_delete : Path.Source.t list
  ; new_file : Path.Source.t
  ; contents : string
  }

type todo =
  { mutable to_rename_and_edit : rename_and_edit list
  ; mutable to_edit : (Path.Source.t * string) list
  }
