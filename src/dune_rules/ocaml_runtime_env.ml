open Import
open Memo.O

let add_defaults
      { Ocaml_toolchain.ocaml_config; _ }
      ~use_small_heap_limit
      ~inherited
      ~overrides
  =
  Ocaml.Runparam.add_defaults
    ~inherited
    ~overrides
    ~word_size:(Ocaml_config.word_size ocaml_config)
    ~use_small_heap_limit
;;

let toolchain sctx ~dir overrides =
  Action_builder.of_memo
    (let+ ocaml = Context.ocaml (Super_context.context sctx)
     and+ inherited = Super_context.context_env_by_dir sctx ~dir in
     add_defaults
       ocaml
       ~use_small_heap_limit:(Ocaml.Version.supports_small_heap_limit ocaml.version)
       ~inherited
       ~overrides)
;;

let melange sctx ~dir =
  Action_builder.of_memo
    (let+ ocaml = Context.ocaml (Super_context.context sctx)
     and+ inherited = Super_context.context_env_by_dir sctx ~dir in
     if Env.mem inherited ~var:(Env.Var.of_string "CAMLRUNPARAM")
     then Env.empty
     else add_defaults ocaml ~use_small_heap_limit:true ~inherited ~overrides:Env.empty)
;;
