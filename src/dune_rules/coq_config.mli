open Import

type t

val version : bin:Path.t -> (string, User_message.Style.t Pp.t) Result.t Memo.t

val make : bin:Path.t -> t Memo.t

module Value : sig
  type t =
    | Bool of bool
    | Int of int
    | String of string
    | Strings of string list
    | Path of Path.t
end

val by_name : t -> string -> Value.t option
