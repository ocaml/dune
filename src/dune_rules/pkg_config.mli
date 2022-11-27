open Import

module Query : sig
  type t =
    | Libs of string
    | Cflags of string

  val file : t -> dir:Path.Build.t -> Path.Build.t

  val read :
    t -> Super_context.t -> dir:Path.Build.t -> string list Action_builder.t
end

val gen_rule :
     Super_context.t
  -> loc:Loc.t
  -> dir:Path.Build.t
  -> Query.t
  -> (unit, [ `Not_found ]) result Memo.t

val read_flags : file:Path.Build.t -> string list Action_builder.t
