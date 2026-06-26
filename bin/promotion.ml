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

(* Promotion subcommands don't build anything: they read the [.to-promote] db
   and copy files between the build dir and source tree. *)
let init_no_build builder =
  Common.Builder.forbid_builds builder |> Common.Builder.disable_log_file |> Common.init
;;

(* Conv for a pending-promotion file: parses as the raw user string (same
   as [Arg.path]), but completes against the entries recorded in the build
   dir's [.to-promote] db.

   Cmdliner passes us the [Common.Builder.t] parsed from the command's own
   term as the completion context, so [--root] / [--build-dir] are honoured.
   Init goes through [init_no_build] — same as the runtime — so no [_build/]
   mkdir and no trace-file rewrite on every keystroke.

   CR-someday Alizter: subtract already-typed positional args from the
   candidate list. Needs cmdliner to pass the previously parsed positional
   values into the completion func (the current public API exposes only
   the context value and the cursor token). *)
let promote_file =
  let parser s = Ok s in
  let pp ppf s = Format.pp_print_string ppf s in
  let candidates ~(builder : Common.Builder.t) ~token =
    let common, _config = init_no_build builder in
    let { Diff_promotion.present; missing = _ } =
      let db = Diff_promotion.load_db () in
      Diff_promotion.partition_db db Dune_rpc.Files_to_promote.All
    in
    let from =
      Path.source (Path.Source.L.relative Path.Source.root (Common.root common).to_cwd)
    in
    List.filter_map present ~f:(fun file ->
      let cwd_relative =
        Path.source (Diff_promotion.File.source file) |> Path.reach ~from
      in
      Option.some_if (String.starts_with ~prefix:token cwd_relative) cwd_relative)
    |> List.sort ~compare:String.compare
    |> List.map ~f:Cmdliner.Arg.Completion.string
  in
  let completion =
    Cmdliner.Arg.Completion.make ~context:Common.Builder.term (fun ctx ~token ->
      let builder = Option.value ctx ~default:Common.Builder.default in
      Ok (candidates ~builder ~token))
  in
  Cmdliner.Arg.Conv.make ~docv:"FILE" ~parser ~pp ~completion ()
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
      ; `P
          {|When FILE arguments are provided, dune promote matches pending
           promotions recursively under those paths. Use $(b,--file) to require
           exact file matching instead.
         |}
      ; `Blocks Common.help_secs
      ]
    in
    Cmd.info ~doc ~man "apply"
  ;;

  let term =
    let+ builder = Common.Builder.term
    and+ exact =
      Arg.(
        value
        & flag
        & info [ "file" ] ~doc:(Some "Require each FILE argument to match exactly."))
    and+ files =
      (* CR-someday Alizter: document this option *)
      Arg.(value & pos_all promote_file [] & info [] ~docv:"FILE" ~doc:None)
    in
    let common, config = init_no_build builder in
    let files_to_promote = files_to_promote ~common files in
    let matching : Dune_rpc.Promote_targets.Matching.t =
      if exact then Exact else Prefix
    in
    match Global_lock.lock () with
    | Ok () ->
      Diff_promotion.promote_files_registered_in_last_run ~matching files_to_promote
      |> on_missing
    | Error lock_held_by ->
      Scheduler_setup.no_build_no_rpc ~config (fun () ->
        let open Fiber.O in
        Rpc.Rpc_common.fire_request
          ~name:"promote_many"
          ~wait:false
          ~lock_held_by
          builder
          Dune_rpc.Procedures.Public.promote_many
          { Dune_rpc.Promote_targets.files = files_to_promote; matching }
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
    let common, config = init_no_build builder in
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
    let common, _config = init_no_build builder in
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
    let common, _config = init_no_build builder in
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
  Common.command_alias ~orig_name:"promotion apply" Apply.command Apply.term "promote"
;;
