open Import

type t

val to_dyn : t -> Dyn.t

val equal : t -> t -> bool

val is_valid_source_path : Path.Source.t -> bool

val of_path_source : Path.Source.t -> (t, [ `Msg of string ]) result

val of_path_source_exn : Path.Source.t -> t

val to_path_source : t -> Path.Source.t

val to_path : t -> Path.t

val of_string : string -> t

val to_string : t -> string
