open Import

module Query : sig
  type t =
    | Libs of string
    | Cflags of string

  val read
    :  loc:Loc.t
    -> t
    -> Super_context.t
    -> dir:Path.Build.t
    -> string list Action_builder.t
end
