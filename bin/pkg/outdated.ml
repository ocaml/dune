open Import
open Pkg_common
module Lock_dir = Dune_pkg.Lock_dir
module Opam_repo = Dune_pkg.Opam_repo

let find_outdated_packages ~transitive ~lock_dirs_arg () =
  let open Fiber.O in
  let+ pps, not_founds =
    let* workspace = Memo.run (Workspace.workspace ()) in
    Pkg_common.Lock_dirs_arg.lock_dirs_of_workspace lock_dirs_arg workspace
    |> Fiber.parallel_map ~f:(fun lock_dir_path ->
      (* updating makes sense when checking for outdated packages *)
      let* repos =
        get_repos
          (repositories_of_workspace workspace)
          ~repositories:(repositories_of_lock_dir workspace ~lock_dir_path)
      and+ local_packages = Memo.run find_local_packages in
      let lock_dir = Lock_dir.read_disk lock_dir_path in
      let+ results = Dune_pkg_outdated.find ~repos ~local_packages lock_dir.packages in
      ( Dune_pkg_outdated.pp ~transitive ~lock_dir_path results
      , ( Dune_pkg_outdated.packages_that_were_not_found results
          |> Package_name.Set.of_list
          |> Package_name.Set.to_list
        , lock_dir_path
        , repos ) ))
    >>| List.split
  in
  (match pps with
   | [ _ ] -> Console.print pps
   | _ -> Console.print [ Pp.enumerate ~f:Fun.id pps ]);
  let error_messages =
    List.filter_map not_founds ~f:(function
      | [], _, _ -> None
      | packages, lock_dir_path, repos ->
        Pp.concat
          ~sep:Pp.space
          [ Pp.textf
              "When checking %s, the following packages:"
              (Path.Source.to_string_maybe_quoted lock_dir_path)
            |> Pp.hovbox
          ; Pp.concat
              ~sep:Pp.space
              [ Pp.enumerate packages ~f:(fun name ->
                  Dune_lang.Package_name.to_string name |> Pp.verbatim)
              ; Pp.text "were not found in the following opam repositories:" |> Pp.hovbox
              ; Pp.enumerate repos ~f:(fun repo ->
                  (* CR-rgrinberg: why are we outputting [Dyn.t] in error
                     messages? *)
                  Opam_repo.serializable repo
                  |> Dyn.option Opam_repo.Serializable.to_dyn
                  |> Dyn.pp)
              ]
            |> Pp.vbox
          ]
        |> Pp.hovbox
        |> Option.some)
  in
  match error_messages with
  | [] -> ()
  | error_messages ->
    User_error.raise (Pp.text "Some packages could not be found." :: error_messages)
;;

let term =
  let+ builder = Common.Builder.term
  and+ transitive =
    Arg.(
      value
      & flag
      & info
          [ "transitive" ]
          ~doc:"Check for outdated packages in transitive dependencies")
  and+ lock_dirs_arg = Pkg_common.Lock_dirs_arg.term in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config @@ find_outdated_packages ~transitive ~lock_dirs_arg
;;

let info =
  let doc = "Check for outdated packages" in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "List packages in from lock directory that have newer versions available. By \
         default, only direct dependencies are checked. The $(b,--transitive) flag can \
         be used to check transitive dependencies as well."
    ; `P "For example:"
    ; `Pre "    \\$ dune pkg outdated"
    ; `Noblank
    ; `Pre "    1/2 packages in dune.lock are outdated."
    ; `Noblank
    ; `Pre "    - ocaml 4.14.1 < 5.1.0"
    ; `Noblank
    ; `Pre "    - dune 3.7.1 < 3.11.0"
    ]
  in
  Cmd.info "outdated" ~doc ~man
;;

let command = Cmd.v info term
