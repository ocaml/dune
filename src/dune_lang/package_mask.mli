open Import

type t = private
  | Inside_package of Package_id.t
  | Forbidden_packages of Package_id.t Path.Source.Map.t

val package_env : dir:Path.Source.t -> packages:Package_id.t Path.Source.Map.t -> t
val key : t Univ_map.Key.t
val decode : Package_id.t option Decoder.t
