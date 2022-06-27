open Import

module Query : sig
  type t =
    | Libs of string
    | Cflags of string
end

val gen_rule :
     Super_context.t
  -> loc:Loc.t
  -> dir:Path.Build.t
  -> Query.t
  -> target:Path.Build.t
  -> (unit, [ `Not_found ]) result Memo.t

val read_flags : file:Path.Build.t -> string list Action_builder.t
