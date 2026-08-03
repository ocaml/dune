open Import

type t

val none : t
val instances : sctx:Super_context.t -> db:Lib.db -> Lib_dep.t list -> t Resolve.Memo.t

(** [ml_source_length t] is the exact number of bytes appended by
    [add_ml_source _ t]. *)
val ml_source_length : t -> int

val add_ml_source : String_builder.t -> t -> unit
