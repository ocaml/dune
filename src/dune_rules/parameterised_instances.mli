open Import

type t

val none : t
val instances : sctx:Super_context.t -> db:Lib.db -> Lib_dep.t list -> t Resolve.Memo.t
val print_instances : Buffer.t -> t -> unit
