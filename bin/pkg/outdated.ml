open Import
open Pkg_common
module Lock_dir = Dune_pkg.Lock_dir
module Opam_repo = Dune_pkg.Opam_repo

let find_outdated_packages
  ~context_name_arg
  ~all_contexts_arg
  ~opam_repository_path
  ~opam_repository_url
  ~transitive
  ()
  =
  let open Fiber.O in
  let+ pps, not_founds =
    Per_context.choose ~context_name_arg ~all_contexts_arg ~version_preference_arg:None
    >>= Fiber.parallel_map
          ~f:
            (fun
              { Per_context.lock_dir_path
              ; version_preference = _
              ; repos
              ; solver_sys_vars = _
              ; context_common = _
              ; repositories
              }
            ->
            (* updating makes sense when checking for outdated packages *)
            let* repos =
              get_repos
                repos
                ~opam_repository_path
                ~opam_repository_url
                ~repositories
                ~update_opam_repositories:true
            and+ local_packages = find_local_packages in
            let lock_dir = Lock_dir.read_disk lock_dir_path in
            let+ results =
              Dune_pkg_outdated.find ~repos ~local_packages lock_dir.packages
            in
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
  and+ context_name_arg = context_term ~doc:"Check for outdated packages in this context"
  and+ all_contexts_arg =
    Arg.(
      value
      & flag
      & info [ "all-contexts" ] ~doc:"Check for outdated packages in all contexts")
  and+ opam_repository_path = Opam_repository_path.term
  and+ opam_repository_url = Opam_repository_url.term
  and+ transitive =
    Arg.(
      value
      & flag
      & info
          [ "transitive" ]
          ~doc:"Check for outdated packages in transitive dependencies")
  in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config
  @@ find_outdated_packages
       ~context_name_arg
       ~all_contexts_arg
       ~opam_repository_path
       ~opam_repository_url
       ~transitive
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
