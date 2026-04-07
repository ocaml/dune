open Import

let latest_lang_version =
  Cmd.v
    (Cmd.info "latest-lang-version")
    (let+ () = Term.const () in
     print_endline
       (Dune_lang.Syntax.greatest_supported_version_exn Stanza.syntax
        |> Dune_lang.Syntax.Version.to_string))
;;

let bootstrap_info =
  let doc = "Print the generated bootstrap info for Dune itself." in
  let info = Cmd.info "bootstrap-info" ~doc in
  let term =
    let+ builder = Common.Builder.term
    and+ context_name = Common.context_arg ~doc:(Some "Build context to use.") in
    let common, config = Common.init builder in
    Scheduler_setup.go_without_rpc_server ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Util.setup () in
      Build.build_memo_exn
      @@ fun () ->
      let open Memo.O in
      let* setup = setup in
      let context = Dune_rules.Main.find_context_exn setup ~name:context_name in
      let file = Path.Build.relative (Context.build_dir context) "bin/bootstrap-info" in
      let* () = Build_system.build_file (Path.build file) in
      let+ () = Memo.return (print_string (Io.read_file (Path.build file))) in
      ())
  in
  Cmd.v info term
;;

module Sexp_pp = struct
  type format =
    | Sexp
    | Csexp

  let format_arg =
    let all = [ "sexp", Sexp; "csexp", Csexp ] in
    let doc = Printf.sprintf "$(docv) must be %s" (Arg.doc_alts_enum all) in
    Arg.(value & opt (enum all) Sexp & info [ "format" ] ~docv:"FORMAT" ~doc:(Some doc))
  ;;

  let version = Dune_lang.Syntax.greatest_supported_version_exn Stanza.syntax

  let print csts =
    Format.fprintf
      Format.std_formatter
      "%a%!"
      Pp.to_fmt
      (Dune_lang.Format.pp_top_sexps ~version csts)
  ;;

  let rec dune_lang_of_sexp : Sexp.t -> Dune_lang.t = function
    | Atom s -> Dune_lang.atom_or_quoted_string s
    | List xs -> List (List.map xs ~f:dune_lang_of_sexp)
  ;;

  let parse_dune_sexps path =
    match path with
    | Some path -> Dune_lang.Parser.load path ~mode:Cst
    | None ->
      Dune_lang.Parser.parse (Lexbuf.from_channel stdin ~fname:"<stdin>") ~mode:Cst
  ;;

  let parse_csexps path =
    let parsed =
      match path with
      | Some path -> Io.with_file_in ~binary:true path ~f:Csexp.input_many
      | None -> Csexp.input_many stdin
    in
    match parsed with
    | Ok sexps ->
      List.map sexps ~f:(fun sexp ->
        sexp
        |> dune_lang_of_sexp
        |> Dune_lang.Ast.add_loc ~loc:Loc.none
        |> Dune_lang.Cst.concrete)
    | Error message ->
      let input =
        match path with
        | Some path -> Path.to_string path
        | None -> "<stdin>"
      in
      User_error.raise [ Pp.textf "failed to parse %s as csexp: %s" input message ]
  ;;

  let command =
    let doc = "Pretty print s-expressions read from stdin or a file." in
    let info = Cmd.info "sexp-pp" ~doc in
    let term =
      let+ format = format_arg
      and+ input =
        let doc = "Read input from this file instead of stdin." in
        Arg.(value & pos 0 (some Arg.path) None & info [] ~docv:"FILE" ~doc:(Some doc))
      in
      let input = Option.map input ~f:Arg.Path.path in
      let csts =
        match format with
        | Sexp -> parse_dune_sexps input
        | Csexp -> parse_csexps input
      in
      print csts
    in
    Cmd.v info term
  ;;
end

let group =
  Cmd.group
    (Cmd.info "internal")
    [ Internal_dump.command
    ; Internal_digest_db.command
    ; latest_lang_version
    ; bootstrap_info
    ; Sexp_pp.command
    ]
;;
