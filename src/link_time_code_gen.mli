(** {1 Handle link time code generation} *)

val libraries_link
  :  name:string
  -> mode:Mode.t
  -> Compilation_context.t
  -> Lib.L.t
  -> _ Arg_spec.t
(** Insert link time generated code for findlib_dynload in the list *)
