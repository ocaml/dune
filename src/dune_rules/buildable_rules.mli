(** Rules and helpers shared by OCaml libraries and executables *)

open Import

(** Make sure all rules produces by [f] record the library dependencies for
    [dune external-lib-deps] and depend on the generation of the .merlin file.

    /!\ WARNING /!\: make sure the last function call inside [f] is fully
    applied, otherwise the function might end up being executed after this
    function has returned. Consider adding a type annotation to make sure this
    doesn't happen by mistake. *)

val gen_select_rules :
  Super_context.t -> dir:Path.Build.t -> Lib.Compile.t -> unit Memo.t

(** Generate the rules for the [(select ...)] forms in library dependencies *)
val with_lib_deps :
     Context.t
  -> Lib.Compile.t
  -> dir:Path.Build.t
  -> f:(unit -> 'a Memo.t)
  -> 'a Memo.t

type kind =
  | Executables of Dune_file.Buildable.t * (Loc.t * string) list
  | Library of Dune_file.Buildable.t * Lib_name.Local.t
  | Melange of
      { preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
      ; preprocessor_deps : Dep_conf.t list
      ; lint : Preprocess.Without_instrumentation.t Preprocess.Per_module.t
      ; empty_module_interface_if_absent : bool
      }

val modules_rules :
     Super_context.t
  -> kind
  -> Expander.t
  -> dir:Path.Build.t
  -> Scope.t
  -> Modules.t
  -> (Modules.t * Pp_spec.t) Memo.t
