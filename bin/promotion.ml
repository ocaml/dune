open Import
module Diff_promotion = Dune_engine.Diff_promotion

let files_to_promote ~common files : Dune_rpc.Files_to_promote.t =
  match files with
  | [] -> All
  | _ ->
    let files =
      List.map files ~f:(fun fn -> Path.Source.of_string (Common.prefix_target common fn))
    in
    These files
;;

let on_missing missing =
  List.sort missing ~compare:Path.Source.compare
  |> List.iter ~f:(fun fn ->
    User_warning.emit
      [ Pp.paragraphf "Nothing to promote for %s." (Path.Source.to_string_maybe_quoted fn)
        |> Pp.tag User_message.Style.Warning
      ])
;;

module Apply = struct
  let info =
    let doc = "Promote files from the last run" in
    let man =
      [ `S Cmdliner.Manpage.s_description
      ; `P
          {|Considering all actions of the form $(b,(diff a b)) that failed
           in the last run of dune, $(b,dune promotion apply) does the following:

           If $(b,a) is present in the source tree but $(b,b) isn't, $(b,b) is
           copied over to $(b,a) in the source tree. The idea behind this is that
           you might use $(b,(diff file.expected file.generated)) and then call
           $(b,dune promote) to promote the generated file.
         |}
      ; `Blocks Common.help_secs
      ]
    in
    Cmd.info ~doc ~man "apply"
  ;;

  let term =
    let+ builder = Common.Builder.term
    and+ files =
      (* CR-someday Alizter: document this option *)
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE" ~doc:None)
    in
    let common, config = Common.init builder in
    let files_to_promote = files_to_promote ~common files in
    match Global_lock.lock ~timeout:None with
    | Ok () ->
      Diff_promotion.promote_files_registered_in_last_run files_to_promote |> on_missing
    | Error lock_held_by ->
      Scheduler_setup.no_build_no_rpc ~config (fun () ->
        let open Fiber.O in
        Rpc.Rpc_common.fire_request
          ~name:"promote_many"
          ~wait:true
          ~lock_held_by
          builder
          Dune_rpc_private.Procedures.Public.promote_many
          files_to_promote
        >>| Rpc.Rpc_common.wrap_build_outcome_exn ~print_on_success:true)
  ;;

  let command = Cmd.v info term
end

module Diff = struct
  let info = Cmd.info ~doc:"List promotions to be applied" "diff"

  let diff_for_file (file : Diff_promotion.File.t) =
    let original = Diff_promotion.File.source file in
    let correction = Diff_promotion.File.correction_file file in
    Dune_engine.Print_diff.get (Path.source original) correction
  ;;

  let display_diffs present =
    let open Fiber.O in
    List.sort present ~compare:Diff_promotion.File.compare
    |> Fiber.parallel_map ~f:(fun file -> diff_for_file file >>| Result.to_option)
    >>| List.filter_opt
    >>| List.iter ~f:Dune_engine.Print_diff.Diff.print
  ;;

  let term =
    let+ builder = Common.Builder.term
    and+ files =
      (* CR-someday Alizter: document this option *)
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE" ~doc:None)
    in
    let common, config = Common.init builder in
    let files_to_promote = files_to_promote ~common files in
    (* CR-soon rgrinberg: remove pointless args *)
    Scheduler_setup.no_build_no_rpc ~config (fun () ->
      let db = Diff_promotion.load_db () in
      let { Diff_promotion.present; missing } =
        Diff_promotion.partition_db db files_to_promote
      in
      on_missing missing;
      display_diffs present)
  ;;

  let command = Cmd.v info term
end

module Files = struct
  let info = Cmd.info ~doc:"List promotions files" "list"

  let term =
    let+ builder = Common.Builder.term
    and+ files =
      (* CR-someday Alizter: document this option *)
      (* CR-soon rgrinberg: why do we even need this argument? *)
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE" ~doc:None)
    in
    let common, _config = Common.init builder in
    let files_to_promote = files_to_promote ~common files in
    (* CR-soon rgrinberg: remove pointless args *)
    let db = Diff_promotion.load_db () in
    let { Diff_promotion.present; missing } =
      Diff_promotion.partition_db db files_to_promote
    in
    on_missing missing;
    List.sort present ~compare:(fun x y ->
      Path.Source.compare (Diff_promotion.File.source x) (Diff_promotion.File.source y))
    |> List.iter ~f:(fun file ->
      Diff_promotion.File.source file |> Path.Source.to_string |> print_endline)
  ;;

  let command = Cmd.v info term
end

module Show = struct
  let info = Cmd.info ~doc:"Display contents of a corrected file" "show"

  let display_corrected_contents db files_to_promote =
    let { Diff_promotion.present; missing = _ } =
      Diff_promotion.partition_db db files_to_promote
    in
    List.iter present ~f:(fun file ->
      let correction_file = Diff_promotion.File.correction_file file in
      if Fpath.exists (Path.to_string correction_file)
      then Io.read_file correction_file |> print_endline
      else
        User_warning.emit
          [ Pp.textf
              "Corrected file does not exist for %s."
              (Diff_promotion.File.source file |> Path.Source.to_string_maybe_quoted)
          ])
  ;;

  let term =
    let+ builder = Common.Builder.term
    and+ files =
      (* CR-someday rgrinberg: should we really allow more than one file? How
         are users supposed to distinguish the output? *)
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE" ~doc:None)
    in
    let common, _config = Common.init builder in
    let files_to_promote = files_to_promote ~common files in
    let db = Diff_promotion.load_db () in
    let { Diff_promotion.present = _; missing } =
      Diff_promotion.partition_db db files_to_promote
    in
    on_missing missing;
    display_corrected_contents db files_to_promote
  ;;

  let command = Cmd.v info term
end

let info =
  Cmd.info ~doc:"Control how changes are propagated back to source code." "promotion"
;;

let group = Cmd.group info [ Files.command; Apply.command; Diff.command; Show.command ]

let promote =
  command_alias ~orig_name:"promotion apply" Apply.command Apply.term "promote"
;;
