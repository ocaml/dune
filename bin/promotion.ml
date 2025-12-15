open Import
module Diff_promotion = Promote.Diff_promotion

let files_to_promote ~common files : Dune_rpc.Files_to_promote.t =
  match files with
  | [] -> All
  | _ ->
    let files =
      List.map files ~f:(fun fn -> Path.Source.of_string (Common.prefix_target common fn))
    in
    These files
;;

let on_missing fn =
  User_warning.emit
    [ Pp.paragraphf "Nothing to promote for %s." (Path.Source.to_string_maybe_quoted fn)
      |> Pp.tag User_message.Style.Warning
    ]
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
    match Dune_util.Global_lock.lock ~timeout:None with
    | Ok () ->
      (* Why are we starting an RPC server??? *)
      Scheduler.go_with_rpc_server ~common ~config (fun () ->
        let open Fiber.O in
        let+ () = Fiber.return () in
        let missing =
          Diff_promotion.promote_files_registered_in_last_run files_to_promote
        in
        List.iter ~f:on_missing missing)
    | Error lock_held_by ->
      Scheduler.no_build_no_rpc ~config (fun () ->
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

  let term =
    let+ builder = Common.Builder.term
    and+ files =
      (* CR-someday Alizter: document this option *)
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE" ~doc:None)
    in
    let common, config = Common.init builder in
    let files_to_promote = files_to_promote ~common files in
    (* CR-soon rgrinberg: remove pointless args *)
    Scheduler.no_build_no_rpc ~config (fun () ->
      let open Fiber.O in
      let db = Diff_promotion.load_db () in
      let* missing = Diff_promotion.missing ~db files_to_promote in
      List.iter ~f:on_missing missing;
      Diff_promotion.display_diffs ~db files_to_promote)
  ;;

  let command = Cmd.v info term
end

module Files = struct
  let info = Cmd.info ~doc:"List promotions files" "list"

  let term =
    let+ builder = Common.Builder.term
    and+ files =
      (* CR-someday Alizter: document this option *)
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE" ~doc:None)
    in
    let common, config = Common.init builder in
    let files_to_promote = files_to_promote ~common files in
    (* CR-soon rgrinberg: remove pointless args *)
    Scheduler.no_build_no_rpc ~config (fun () ->
      let open Fiber.O in
      let db = Diff_promotion.load_db () in
      let* missing = Diff_promotion.missing ~db files_to_promote in
      List.iter ~f:on_missing missing;
      Diff_promotion.display_files ~db files_to_promote)
  ;;

  let command = Cmd.v info term
end

module Show = struct
  let info = Cmd.info ~doc:"Display contents of a corrected file" "show"

  let term =
    let+ builder = Common.Builder.term
    and+ files =
      Arg.(value & pos_all Cmdliner.Arg.file [] & info [] ~docv:"FILE" ~doc:None)
    in
    let common, config = Common.init builder in
    let files_to_promote = files_to_promote ~common files in
    (* CR-soon rgrinberg: remove pointless args *)
    Scheduler.no_build_no_rpc ~config (fun () ->
      let open Fiber.O in
      let db = Diff_promotion.load_db () in
      let+ missing = Diff_promotion.missing ~db files_to_promote in
      List.iter ~f:on_missing missing;
      Diff_promotion.display_corrected_contents ~db files_to_promote)
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
