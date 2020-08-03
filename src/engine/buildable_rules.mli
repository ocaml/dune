(** Rules and helpers shared by OCaml libraries and executables *)

open Stdune

(** Make sure all rules produces by [f] record the library dependencies for
    [dune external-lib-deps] and depend on the generation of the .merlin file.

    /!\ WARNING /!\: make sure the last function call inside [f] is fully
    applied, otherwise the function might end up being executed after this
    function has returned. Consider adding a type annotation to make sure this
    doesn't happen by mistake. *)

val gen_select_rules :
  Super_context.t -> dir:Path.Build.t -> Lib.Compile.t -> unit

(** Generate the rules for the [(select ...)] forms in library dependencies *)
val with_lib_deps :
  Context.t -> Lib.Compile.t -> dir:Path.Build.t -> f:(unit -> 'a) -> 'a
