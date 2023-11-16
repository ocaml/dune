open Import
open Memo.O

let header_file_content =
  {|
#if defined( __clang__ )
  #define CCOMP clang
#elif defined( _MSC_VER )
  #define CCOMP msvc
#elif defined( __GNUC__ )
  #define CCOMP gcc
#else
  #define CCOMP other
#endif

CCOMP
|}
;;

let rules ~sctx ~dir =
  let file = Path.Build.relative dir Cxx_flags.preprocessed_filename in
  let ocfg =
    Action_builder.of_memo
    @@
    let+ ocaml = Super_context.context sctx |> Context.ocaml in
    ocaml.ocaml_config
  in
  (* let tmp = Path.External.of_string (Filename.get_temp_dir_name ()) in *)
  let header_file = Path.Build.relative dir "header_check.h" in
  let write_test_file = Action.write_file header_file header_file_content in
  let args =
    let open Action_builder.O in
    Command.Args.Dyn
      (let+ ocfg = ocfg in
       let open Command.Args in
       S
         [ (match Ocaml_config.ccomp_type ocfg with
            | Msvc -> A "/EP"
            | Other _ -> As [ "-E"; "-P" ])
         ; Path (Path.build header_file)
         ])
  in
  let action =
    let prog =
      let open Action_builder.O in
      let* ocfg = ocfg in
      Super_context.resolve_program sctx ~dir ~loc:None (Ocaml_config.c_compiler ocfg)
    in
    let open Action_builder.With_targets.O in
    let+ run_preprocessor =
      Command.run_dyn_prog ~dir:(Path.build dir) ~stdout_to:file prog [ args ]
    in
    Action.Full.reduce [ Action.Full.make write_test_file; run_preprocessor ]
  in
  Super_context.add_rule sctx ~dir action
;;
