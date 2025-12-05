open Import
open Dune_rules

module Test_description = struct
  type t =
    { name : string
    ; source_dir : string
    ; package : string option
    ; enabled : bool
    ; location : string
    ; target : string
    }

  let to_dyn { name; source_dir; package; enabled; location; target } =
    let open Dyn in
    record
      [ "name", string name
      ; "source_dir", string source_dir
      ; "package", option string package
      ; "enabled", bool enabled
      ; "location", string location
      ; "target", string target
      ]
  ;;
end

module Crawl = struct
  open Memo.O

  (* Collect all (stanza, dir, expander) for test-related stanzas *)
  let collect_test_stanzas
        ({ Import.Main.contexts = _; scontexts } : Import.Main.build_system)
        (context : Context.t)
    : (Stanza.t * Path.Build.t * Expander.t) list Memo.t
    =
    let context_name = Context.name context in
    let sctx = Context_name.Map.find_exn scontexts context_name in
    let* dune_files = Dune_load.dune_files context_name in
    Memo.parallel_map dune_files ~f:(fun (dune_file : Dune_file.t) ->
      Dune_file.stanzas dune_file
      >>= fun stanzas ->
      let dir =
        Path.Build.append_source (Context.build_dir context) (Dune_file.dir dune_file)
      in
      let* expander = Super_context.expander sctx ~dir in
      Memo.return
        (List.filter_map stanzas ~f:(fun stanza ->
           match Stanza.repr stanza with
           | Tests.T _ | Cram_stanza.T _ -> Some (stanza, dir, expander)
           | Library.T lib ->
             (match
                Sub_system_name.Map.find lib.sub_systems Inline_tests_info.Tests.name
              with
              | Some (Dune_rules.Inline_tests_info.Tests.T _) ->
                Some (stanza, dir, expander)
              | _ -> None)
           | _ -> None)))
    >>| List.concat
  ;;

  (* Transform a stanza into a list of Test_description.t *)
  let describe_stanza stanza dir expander : Test_description.t list Memo.t =
    match Stanza.repr stanza with
    | Tests.T (tests : Tests.t) ->
      let* enabled = Expander.eval_blang expander tests.enabled_if in
      let names = List.map ~f:snd (Nonempty_list.to_list tests.exes.names) in
      let package =
        Option.map tests.package ~f:(fun p ->
          Dune_lang.Package.name p |> Dune_lang.Package_name.to_string)
      in
      let location = Loc.to_file_colon_line tests.exes.buildable.loc in
      let source_dir = Path.Build.drop_build_context_exn dir |> Path.Source.to_string in
      let descs =
        List.map names ~f:(fun name ->
          let target = Path.Build.relative dir (name ^ ".exe") |> Path.Build.to_string in
          { Test_description.name; source_dir; package; enabled; location; target })
      in
      Memo.return descs
    | Cram_stanza.T cram ->
      let* enabled = Expander.eval_blang expander cram.enabled_if in
      let package =
        Option.map cram.package ~f:(fun p ->
          Dune_lang.Package.name p |> Dune_lang.Package_name.to_string)
      in
      let location = Loc.to_file_colon_line cram.loc in
      let source_dir = Path.Build.drop_build_context_exn dir |> Path.Source.to_string in
      let name = "cram" in
      (* Use the runtest alias as target, which is the actual executable target *)
      let target = "@" ^ source_dir ^ "/runtest" in
      let description =
        { Test_description.name; source_dir; package; enabled; location; target }
      in
      Memo.return [ description ]
    | Library.T lib ->
      let* enabled =
        let inline_tests =
          match Sub_system_name.Map.find lib.sub_systems Inline_tests_info.Tests.name with
          | Some (Dune_rules.Inline_tests_info.Tests.T t) -> t
          | _ -> assert false
        in
        Expander.eval_blang expander inline_tests.enabled_if
      in
      let name = Lib_name.Local.to_string (snd lib.name) in
      let package =
        Option.map (Library.package lib) ~f:(fun p ->
          Dune_lang.Package.name p |> Dune_lang.Package_name.to_string)
      in
      let location = Loc.to_file_colon_line lib.buildable.loc in
      let source_dir = Path.Build.drop_build_context_exn dir |> Path.Source.to_string in
      let target =
        "@" ^ source_dir ^ "/runtest-" ^ Lib_name.Local.to_string (snd lib.name)
      in
      let description =
        { Test_description.name; source_dir; package; enabled; location; target }
      in
      Memo.return [ description ]
    | _ -> Memo.return []
  ;;

  (* Main entry: crawl and describe all test stanzas *)
  let tests build_system context : Test_description.t list Memo.t =
    let* stanzas = collect_test_stanzas build_system context in
    Memo.parallel_map stanzas ~f:(fun (stanza, dir, expander) ->
      describe_stanza stanza dir expander)
    >>| List.concat
  ;;
end

let term : unit Term.t =
  let+ builder = Common.Builder.term
  and+ context_name = Common.context_arg ~doc:(Some "Build context to use.")
  and+ format = Describe_format.arg in
  let common, config = Common.init builder in
  Scheduler.go_with_rpc_server ~common ~config
  @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  build_exn
  @@ fun () ->
  let open Memo.O in
  let* setup = setup in
  let super_context = Import.Main.find_scontext_exn setup ~name:context_name in
  let context = Super_context.context super_context in
  let* tests_data = Crawl.tests setup context in
  let dyn_data =
    List.map tests_data ~f:Test_description.to_dyn |> fun list -> Dyn.List list
  in
  Describe_format.print_dyn format dyn_data;
  Memo.return ()
;;

let command =
  let doc =
    "Print out the tests defined in the project. The output format of this command is \
     experimental and is subject to change without warning"
  in
  let info = Cmd.info ~doc "tests" in
  Cmd.v info term
;;
