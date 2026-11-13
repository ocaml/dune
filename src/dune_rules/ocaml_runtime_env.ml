open Memo.O

let mb = 1024 * 1024
let minor_heap_size = 2 * mb

(* Convert the nominal 128 MiB target using the compiler's word size below.
   The runtime counts GC-work units; this is not a byte limit. *)
let small_heap_limit_bytes = 128 * mb

let toolchain sctx ~dir overrides =
  Action_builder.of_memo
    (let+ ocaml = Context.ocaml (Super_context.context sctx)
     and+ inherited = Super_context.context_env_by_dir sctx ~dir in
     Ocaml.Runparam.add_defaults
       ~inherited
       ~overrides
       ~minor_heap_size
       ~small_heap_limit:
         (if Ocaml.Version.supports_small_heap_limit ocaml.version
          then (
            let bytes_per_word = Ocaml_config.word_size ocaml.ocaml_config / 8 in
            Some (small_heap_limit_bytes / bytes_per_word))
          else None))
;;
