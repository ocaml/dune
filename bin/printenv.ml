open Import

let dump sctx ~dir =
  (* TODO all of this printing functions should probably be inlined here as
     well *)
  let module Env_node = Dune_rules.Env_node in
  let module Link_flags = Dune_rules.Link_flags in
  let module Ocaml_flags = Dune_rules.Ocaml_flags in
  let module Js_of_ocaml = Dune_rules.Js_of_ocaml in
  let node = Super_context.env_node sctx ~dir in
  let open Memo.O in
  let ocaml_flags = node >>= Env_node.ocaml_flags in
  let foreign_flags = node >>| Env_node.foreign_flags in
  let link_flags = node >>= Env_node.link_flags in
  let menhir_flags = node >>| Env_node.menhir_flags in
  let coq_flags = node >>= Env_node.coq in
  let js_of_ocaml = node >>= Env_node.js_of_ocaml in
  let open Action_builder.O in
  let+ o_dump =
    let* ocaml_flags = Action_builder.of_memo ocaml_flags in
    Ocaml_flags.dump ocaml_flags
  and+ c_dump =
    let* foreign_flags = Action_builder.of_memo foreign_flags in
    let+ c_flags = foreign_flags.c
    and+ cxx_flags = foreign_flags.cxx in
    List.map
      ~f:Dune_lang.Encoder.(pair string (list string))
      [ "c_flags", c_flags; "cxx_flags", cxx_flags ]
  and+ link_flags_dump =
    let* link_flags = Action_builder.of_memo link_flags in
    Link_flags.dump link_flags
  and+ menhir_dump =
    let+ flags = Action_builder.of_memo_join menhir_flags in
    [ "menhir_flags", flags ] |> List.map ~f:Dune_lang.Encoder.(pair string (list string))
  and+ coq_dump =
    let+ flags = Action_builder.of_memo_join coq_flags in
    [ "coq_flags", flags ] |> List.map ~f:Dune_lang.Encoder.(pair string (list string))
  and+ jsoo_dump =
    let* jsoo = Action_builder.of_memo js_of_ocaml in
    Js_of_ocaml.Flags.dump jsoo.flags
  in
  let env =
    List.concat [ o_dump; c_dump; link_flags_dump; menhir_dump; coq_dump; jsoo_dump ]
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
      let version = Dune_lang.Syntax.greatest_supported_version Stanza.syntax in
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
    Build_system.run_exn (fun () ->
      let open Memo.O in
      let+ res, _facts = Action_builder.run request Eager in
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
  let doc = "Print the environment of a directory" in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|$(b,dune show env DIR) prints the environment of a directory|}
    ; `Blocks Common.help_secs
    ]
  in
  Cmd.v (Cmd.info "env" ~doc ~man) term
;;
