(** {1 Handle link time code generation} *)

open Stdune

(** Insert link time generated code for findlib_dynload in the list *)
val libraries_link : Compilation_context.t -> (Mode.t -> _ Arg_spec.t) Staged.t
