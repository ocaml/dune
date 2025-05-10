open Import

type t =
  | File of Path.Source.t
  | Dir of
      { file : Path.Source.t
      ; dir : Path.Source.t
      }

val is_cram_suffix : string -> bool
val dyn_of_t : t -> Dyn.t
val name : t -> string
val fname_in_dir_test : Filename.t
val script : t -> Path.Source.t
