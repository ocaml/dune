open Import

module Var : sig
  type t = string
end

module Map : Map.S with type key = Var.t

val initial_env : string array Lazy.t

val extend_env : vars:string Map.t -> env:string array -> string array

val get_env : string array -> string -> string option
