open Import

val copy
  :  src:Path.t
  -> dst:Path.t
  -> copy_file:(src:Path.t -> dst:Path.t -> unit)
  -> mkdir:(src:Path.t -> dst:Path.t -> unit)
  -> on_unsupported:(src:Path.t -> File_kind.t -> unit)
  -> ?on_symlink:[ `Ignore | `Raise | `Call of src:Path.t -> Unix.file_kind option ]
  -> unit
  -> unit
