open Import

module Value : sig
  type t
end

type t

val version : bin:Path.t -> string option Memo.t

val by_name : t -> string -> Value.t option

