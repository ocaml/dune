open Stdune
(** Make sure all rules produces by [f] record the library dependencies for
    [dune external-lib-deps] and depend on the generation of the .merlin file.

    /!\ WARNING /!\: make sure the last function call inside [f] is
    fully applied, otherwise the function might end up being executed
    after this function has returned. Consider addin a type
    annotation to make sure this doesn't happen by mistake.
*)

val gen_select_rules : Rule_context.t -> dir:Path.t -> Lib.Compile.t -> unit

val with_lib_deps
  :  Rule_context.t
  -> Lib.Compile.t
  -> dir:Path.t
  -> f:(unit -> 'a)
  -> 'a
