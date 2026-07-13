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
      Build.build_memo_exn
      @@ fun () ->
      let open Memo.O in
      let* setup = Util.setup () in
      let context = Dune_rules.Main.find_context_exn setup ~name:context_name in
      let file = Path.Build.relative (Context.build_dir context) "bin/bootstrap-info" in
      let* () = Build_system.build_file (Path.build file) in
      let+ () = Memo.return (print_string (Io.read_file (Path.build file))) in
      ())
  in
  Cmd.v info term
;;

module Sexp_io = struct
  let input_arg =
    let doc = "Read input from this file instead of stdin." in
    Arg.(value & pos 0 (some Arg.path) None & info [] ~docv:"FILE" ~doc:(Some doc))
  ;;

  let input_path path = Option.map path ~f:Arg.Path.path

  let parse_dune_sexps path =
    match path with
    | Some path -> Dune_lang.Parser.load path ~mode:Cst
    | None ->
      Dune_lang.Parser.parse (Lexbuf.from_channel stdin ~fname:"<stdin>") ~mode:Cst
  ;;

  let rec dune_lang_of_sexp : Sexp.t -> Dune_lang.t = function
    | Atom s -> Dune_lang.atom_or_quoted_string s
    | List xs -> List (List.map xs ~f:dune_lang_of_sexp)
  ;;

  let rec sexp_of_dune_lang : Dune_lang.t -> Sexp.t = function
    | Atom atom -> Atom (Dune_lang.Atom.to_string atom)
    | Quoted_string s -> Atom s
    | List xs -> List (List.map xs ~f:sexp_of_dune_lang)
    | Template _ -> User_error.raise [ Pp.text "templates cannot be converted to csexps" ]
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
end

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

  type style =
    | Pretty
    | Compact

  let style_arg =
    let doc = "Print compact, one-line s-expressions." in
    Arg.(value & flag & info [ "compact" ] ~doc:(Some doc))
  ;;

  module Stable = struct
    let rec can_be_displayed_inline = function
      | Dune_lang.Atom _ | Quoted_string _ | Template _ | List [] -> true
      | List [ sexp ] -> can_be_displayed_inline sexp
      | List _ -> false
    ;;

    let can_be_displayed_inline sexps =
      List.length sexps <= 2 && List.for_all sexps ~f:can_be_displayed_inline
    ;;

    let rec pp_sexp t =
      let open Pp.O in
      match t with
      | Dune_lang.Atom _ | Quoted_string _ | Template _ ->
        Pp.verbatim (Dune_lang.to_string t)
      | List sexps ->
        let inner = Pp.concat_map sexps ~sep:Pp.space ~f:pp_sexp in
        if can_be_displayed_inline sexps
        then Pp.hbox (Pp.char '(' ++ inner ++ Pp.char ')')
        else
          Pp.vbox
            ~indent:1
            (Pp.char '(' ++ Pp.concat_map sexps ~sep:Pp.cut ~f:pp_sexp ++ Pp.char ')')
    ;;

    let pp_top_sexp sexp =
      let open Pp.O in
      pp_sexp sexp ++ Pp.char '\n'
    ;;

    let pp_top_sexps = Pp.concat_map ~sep:Pp.newline ~f:pp_top_sexp
  end

  let print_pretty format csts =
    let ppf = Format.std_formatter in
    Format.pp_set_margin ppf 78;
    match format with
    | Sexp ->
      Format.fprintf ppf "%a%!" Pp.to_fmt (Dune_lang.Format.pp_top_sexps ~version csts)
    | Csexp ->
      let sexps = List.filter_map csts ~f:Dune_lang.Cst.to_sexp in
      Format.fprintf ppf "%a%!" Pp.to_fmt (Stable.pp_top_sexps sexps)
  ;;

  let print_compact csts =
    List.iter csts ~f:(fun cst ->
      match Dune_lang.Cst.to_sexp cst with
      | None -> ()
      | Some sexp -> print_endline (Dune_lang.to_string sexp))
  ;;

  let print format style csts =
    match style with
    | Pretty -> print_pretty format csts
    | Compact -> print_compact csts
  ;;

  let command =
    let doc = "Pretty print s-expressions read from stdin or a file." in
    let info = Cmd.info "sexp-pp" ~doc in
    let term =
      let+ format = format_arg
      and+ compact = style_arg
      and+ input = Sexp_io.input_arg in
      let input = Sexp_io.input_path input in
      let csts =
        match format with
        | Sexp -> Sexp_io.parse_dune_sexps input
        | Csexp -> Sexp_io.parse_csexps input
      in
      let style = if compact then Compact else Pretty in
      print format style csts
    in
    Cmd.v info term
  ;;
end

module Sexp_to_csexp = struct
  let command =
    let doc = "Convert s-expressions read from stdin or a file to csexps." in
    let info = Cmd.info "sexp-to-csexp" ~doc in
    let term =
      let+ input = Sexp_io.input_arg in
      let input = Sexp_io.input_path input in
      let csts = Sexp_io.parse_dune_sexps input in
      List.iter csts ~f:(fun cst ->
        match Dune_lang.Cst.to_sexp cst with
        | None -> ()
        | Some sexp -> Csexp.to_channel stdout (Sexp_io.sexp_of_dune_lang sexp));
      flush stdout
    in
    Cmd.v info term
  ;;
end

module Cache_metadata = struct
  module Metadata_file = Dune_cache.Local.Artifacts.Metadata_file
  module Restore_result = Dune_cache.Local.Restore_result

  let command =
    let doc = "Print a cache metadata file as JSON." in
    let info = Cmd.info "cache-metadata" ~doc in
    let term =
      let+ input =
        let doc = "Read cache metadata from this file." in
        Arg.(required & pos 0 (some Arg.path) None & info [] ~docv:"FILE" ~doc:(Some doc))
      in
      let input = Arg.Path.path input in
      match Metadata_file.load input with
      | Restored metadata ->
        Json.of_repr Metadata_file.repr metadata |> Json.to_string |> print_endline
      | Not_found_in_cache ->
        User_error.raise
          [ Pp.textf "cache metadata not found: %s" (Path.to_string input) ]
      | Error exn -> User_error.raise [ Pp.textf "%s" (Printexc.to_string exn) ]
    in
    Cmd.v info term
  ;;
end

let group =
  Cmd.group
    (Cmd.info "internal")
    [ Internal_dump.command
    ; Internal_digest_db.command
    ; Internal_action_runner.group
    ; Bwrap.With_bwrap.command
    ; Bwrap.With_sandbox_exec.command
    ; latest_lang_version
    ; bootstrap_info
    ; Sexp_pp.command
    ; Sexp_to_csexp.command
    ; Cache_metadata.command
    ; Shell.Internal_replay.command
    ]
;;
