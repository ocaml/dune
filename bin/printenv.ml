open Import

let dump sctx ~dir =
  (* TODO all of this printing functions should probably be inlined here as
     well *)
  let module Env_node = Dune_rules.Env_node in
  let module Link_flags = Dune_rules.Link_flags in
  let module Ocaml_flags = Dune_rules.Ocaml_flags in
  let open Action_builder.O in
  let+ o_dump =
    Dune_rules.Ocaml_flags_db.ocaml_flags_env ~dir
    |> Action_builder.of_memo
    >>= Ocaml_flags.dump
  and+ c_dump =
    let* foreign_flags =
      Dune_rules.Foreign_rules.foreign_flags_env ~dir |> Action_builder.of_memo
    in
    let+ c_flags = foreign_flags.c
    and+ cxx_flags = foreign_flags.cxx in
    List.map
      ~f:Dune_lang.Encoder.(pair string (list string))
      [ "c_flags", c_flags; "cxx_flags", cxx_flags ]
  and+ link_flags_dump =
    Action_builder.of_memo (Dune_rules.Ocaml_flags_db.link_env ~dir) >>= Link_flags.dump
  and+ menhir_dump =
    Dune_rules.Menhir_rules.menhir_env ~dir
    |> Action_builder.of_memo
    >>= Dune_rules.Menhir_env.dump
  and+ coq_dump =
    Dune_rules.Coq.Coq_rules.coq_env ~dir
    >>| Dune_rules.Coq.Coq_flags.dump ~dir:(Path.build dir)
  and+ jsoo_js_dump =
    let module Js_of_ocaml = Dune_rules.Js_of_ocaml in
    let* jsoo = Action_builder.of_memo (Dune_rules.Jsoo_rules.jsoo_env ~dir ~mode:JS) in
    Js_of_ocaml.Flags.dump ~mode:JS jsoo.flags
  and+ jsoo_wasm_dump =
    let module Js_of_ocaml = Dune_rules.Js_of_ocaml in
    let* jsoo = Action_builder.of_memo (Dune_rules.Jsoo_rules.jsoo_env ~dir ~mode:Wasm) in
    Js_of_ocaml.Flags.dump ~mode:Wasm jsoo.flags
  in
  let env =
    List.concat
      [ o_dump
      ; c_dump
      ; link_flags_dump
      ; menhir_dump
      ; coq_dump
      ; jsoo_js_dump
      ; jsoo_wasm_dump
      ]
  in
  Super_context.context sctx |> Context.name, env
;;

let pp ppf ~fields sexps =
  let fields = String.Set.of_list fields in
  List.iter sexps ~f:(fun sexp ->
    let do_print =
      String.Set.is_empty fields
      ||
      match sexp with
      | Dune_lang.List (Atom (A name) :: _) -> String.Set.mem fields name
      | _ -> false
    in
    if do_print
    then (
      let version = Dune_lang.Syntax.greatest_supported_version_exn Stanza.syntax in
      Dune_lang.Ast.add_loc sexp ~loc:Loc.none
      |> Dune_lang.Cst.concrete
      |> List.singleton
      |> Dune_lang.Format.pp_top_sexps ~version
      |> Format.fprintf ppf "%a@?" Pp.to_fmt))
;;

let term =
  let+ builder = Common.Builder.term
  and+ dir = Arg.(value & pos 0 dir "" & info [] ~docv:"PATH")
  and+ fields =
    Arg.(
      value
      & opt_all string []
      & info
          [ "field" ]
          ~docv:"FIELD"
          ~doc:
            "Only print this field. This option can be repeated multiple times to print \
             multiple fields.")
  in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config (fun () ->
    let open Fiber.O in
    let* setup = Import.Main.setup () in
    let* setup = Memo.run setup in
    let dir = Path.of_string dir in
    let checked = Util.check_path setup.contexts dir in
    let request =
      Action_builder.all
        (match checked with
         | In_build_dir (ctx, _) ->
           let sctx =
             Dune_engine.Context_name.Map.find_exn setup.scontexts (Context.name ctx)
           in
           [ dump sctx ~dir:(Path.as_in_build_dir_exn dir) ]
         | In_source_dir dir ->
           Dune_engine.Context_name.Map.values setup.scontexts
           |> List.map ~f:(fun sctx ->
             let dir =
               Path.Build.append_source
                 (Context.build_dir (Super_context.context sctx))
                 dir
             in
             dump sctx ~dir)
         | In_private_context _ | External _ ->
           User_error.raise [ Pp.text "Environment is not defined for external paths" ]
         | In_install_dir _ ->
           User_error.raise [ Pp.text "Environment is not defined in install dirs" ])
    in
    build_exn (fun () ->
      let open Memo.O in
      let+ res, _facts = Action_builder.evaluate_and_collect_facts request in
      res)
    >>| function
    | [ (_, env) ] -> Format.printf "%a" (pp ~fields) env
    | l ->
      List.iter l ~f:(fun (name, env) ->
        Format.printf
          "@[<v2>Environment for context %s:@,%a@]@."
          (Dune_engine.Context_name.to_string name)
          (pp ~fields)
          env))
;;

let command =
  let doc = "Print the environment of a directory." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|$(b,dune show env DIR) prints the environment of a directory|}
    ; `Blocks Common.help_secs
    ]
  in
  Cmd.v (Cmd.info "env" ~doc ~man) term
;;
