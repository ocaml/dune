open Import
open Memo.O

let mb = 1024 * 1024
let minor_heap_size = 2 * mb

(* Convert the nominal 128 MiB target using the compiler's word size below.
   The runtime counts GC-work units; this is not a byte limit. *)
let small_heap_limit_bytes = 128 * mb

let add_defaults
      { Ocaml_toolchain.ocaml_config; _ }
      ~supports_small_heap_limit
      ~inherited
      ~overrides
  =
  Ocaml.Runparam.add_defaults
    ~inherited
    ~overrides
    ~minor_heap_size
    ~small_heap_limit:
      (if supports_small_heap_limit
       then (
         let bytes_per_word = Ocaml_config.word_size ocaml_config / 8 in
         Some (small_heap_limit_bytes / bytes_per_word))
       else None)
;;

let toolchain sctx ~dir overrides =
  Action_builder.of_memo
    (let+ ocaml = Context.ocaml (Super_context.context sctx)
     and+ inherited = Super_context.context_env_by_dir sctx ~dir in
     add_defaults
       ocaml
       ~supports_small_heap_limit:(Ocaml.Version.supports_small_heap_limit ocaml.version)
       ~inherited
       ~overrides)
;;

let melange sctx ~dir compiler =
  Action_builder.of_memo
    (let* inherited = Super_context.context_env_by_dir sctx ~dir in
     if
       Env.mem inherited ~var:(Env.Var.of_string "OCAMLRUNPARAM")
       || Env.mem inherited ~var:(Env.Var.of_string "CAMLRUNPARAM")
     then Memo.return Env.empty
     else (
       match compiler with
       | Error _ -> Memo.return Env.empty
       | Ok compiler ->
         let ctx = Super_context.context sctx in
         let+ ocaml = Context.ocaml ctx
         and+ supports_small_heap_limit =
           Melange_binary.supports_small_heap_limit
             compiler
             ~dir:(Context.build_dir ctx)
             ~env:inherited
         in
         add_defaults ocaml ~supports_small_heap_limit ~inherited ~overrides:Env.empty))
;;
