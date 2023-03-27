module External_name : sig
  type t

  val of_string : string -> t

  val to_string : t -> string
end

type t =
  | Local of Module.t
  | External of External_name.t

val to_local : Module.t -> t

val to_external : External_name.t -> t

val is_local : t -> bool

val is_external : t -> bool

val filter_local : t -> Module.t option

val filter_external : t -> External_name.t option
