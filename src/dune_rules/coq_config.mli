open Import

type t

val version : bin:Path.t -> (int * int * int) Memo.t

val make : bin:Path.t -> t Memo.t

module Value : sig
  type t =
    | Bool of bool
    | String of string
    | Path of Path.t
    | Paths of Path.t list
    | Version of int * int * int
end

val by_name : t -> string -> Value.t option
