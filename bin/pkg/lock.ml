open Import

let show_solution_for lock_dir =
  let ctx_name = Context_name.default in
  Console.print [ Pp.textf "Solution for %s:" (Path.Source.to_string lock_dir) ];
  let build_lock_dir =
    Path.Build.L.relative
      Dune_rules.Private_context.t.build_dir
      [ Context_name.to_string ctx_name; ".lock" ]
  in
  let source_local = Path.Source.to_local lock_dir in
  let solution_dir = Path.Build.append_local build_lock_dir source_local in
  match Readdir.read_directory (Path.Build.to_string solution_dir) with
  | Ok entries ->
    entries
    |> List.sort ~compare:String.compare
    |> List.filter_map ~f:(fun entry ->
      match String.rsplit2 ~on:'.' entry with
      | Some (pkg_name, "pkg") -> Some (pkg_name, entry)
      | None | Some _ -> None)
    |> (function
     | [] -> Console.print [ Pp.text "(no dependencies to lock)" ]
     | lock_files ->
       lock_files
       |> List.filter_map ~f:(fun (pkg_name, filename) ->
         let path = Path.Build.relative solution_dir filename |> Path.build in
         let mode = Dune_sexp.Parser.Mode.Many in
         let pkg_file = Dune_sexp.Parser.load path ~mode in
         List.find_map pkg_file ~f:(fun item ->
           match (item : Dune_sexp.Ast.t) with
           | List (_, [ Atom (_, A "version"); Atom (_, A ver) ]) ->
             Some (Pp.textf "- %s.%s" pkg_name ver)
           | _ -> None))
       |> Console.print)
  | Error (ENOENT, _, _) -> Console.print [ Pp.text "(no solution found)" ]
  | Error detailed ->
    Code_error.raise
      "Issue checking lock directory"
      [ "error", Unix_error.Detailed.to_dyn detailed ]
;;

let show_solution () =
  let open Memo.O in
  let* workspace = Workspace.workspace () in
  let+ lock_dirs = Dune_rules.Lock_dir.lock_dirs_of_workspace workspace in
  lock_dirs
  |> Path.Source.Set.to_list
  |> List.sort ~compare:Path.Source.compare
  |> List.iter ~f:show_solution_for
;;

let run_lock_command ~(common : Common.t) ~config =
  let open Fiber.O in
  let once () =
    let request (setup : Import.Main.build_system) =
      let dir = Path.(relative root) (Common.prefix_target common ".") in
      let open Action_builder.O in
      let* () =
        Alias.in_dir
          ~name:Dune_rules.Alias.pkg_lock
          ~recursive:true
          ~contexts:setup.contexts
          dir
        |> Alias.request
      in
      Action_builder.of_memo (Memo.of_thunk show_solution)
    in
    Build.run_build_system ~common ~request
    >>| function
    | Ok () -> ()
    | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
  in
  Scheduler.go_with_rpc_server ~common ~config once
;;

let doc = "Create a lock directory."

let man =
  [ `S "DESCRIPTION"
  ; `P {|$(b,dune pkg lock) runs the solver and prints the solution.|}
  ; `P {|Equivalent to $(b,dune build @pkg-lock).|}
  ; `Blocks Common.help_secs
  ]
;;

let command =
  let term =
    let+ builder = Common.Builder.term in
    let common, config = Common.init builder in
    run_lock_command ~common ~config
  in
  Cmd.v (Cmd.info "lock" ~doc ~man ~envs:Common.envs) term
;;
