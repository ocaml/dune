open Import

let get_default_lock_dir_path () = Dune_rules.Lock_dir.default_source_path |> Path.source

let packages_in_repo ~query repo =
  let package_names = Opam_repo.packages_in_repo repo in
  let filtered_packages =
    match query with
    | None -> package_names
    | Some re ->
      List.filter package_names ~f:(fun name ->
        Re.execp re (OpamPackage.Name.to_string name))
  in
  filtered_packages
  |> Fiber.parallel_map ~f:(fun name ->
    let open Fiber.O in
    let* versions = Opam_repo.load_all_versions [ repo ] name in
    let versions =
      OpamPackage.Version.Map.filter
        (fun _version resolved_pkg ->
           let opam_file = Resolved_package.opam_file resolved_pkg in
           let avoid = List.mem opam_file.flags Pkgflag_AvoidVersion ~equal:Poly.equal in
           not avoid)
        versions
    in
    Fiber.return (name, OpamPackage.Version.Map.max_binding_opt versions))
;;

let search_packages ~query () =
  let open Fiber.O in
  let* workspace = Memo.run (Workspace.workspace ()) in
  let* lock_dir_path = Dune_rules.Lock_dir.get_path Context_name.default |> Memo.run in
  let lock_dir_path =
    match lock_dir_path with
    | Some p -> p
    | None -> get_default_lock_dir_path ()
  in
  let* repos =
    Dune_pkg.Opam_repo.resolve_repositories
      ~available_repos:(Pkg_common.repositories_of_workspace workspace)
      ~repositories:(Pkg_common.repositories_of_lock_dir workspace ~lock_dir_path)
  in
  let re = Option.map ~f:(fun q -> Re.str q |> Re.no_case |> Re.compile) query in
  let* filtered = Fiber.parallel_map ~f:(packages_in_repo ~query:re) repos in
  let results =
    filtered
    |> List.concat
    |> List.filter_map ~f:(fun (name, version_pkg) ->
      Option.map ~f:(fun data -> name, data) version_pkg)
    |> List.fold_left
         ~init:OpamPackage.Name.Map.empty
         ~f:(fun acc (name, ((v2, _pkg) as data)) ->
           if OpamPackage.Name.Map.mem name acc
           then (
             let v1, _ = OpamPackage.Name.Map.find name acc in
             (* Replace with next repository's package only if it's a newer
             version, to respect the repository order. *)
             if OpamPackage.Version.compare v1 v2 < 0
             then OpamPackage.Name.Map.add name data acc
             else acc)
           else OpamPackage.Name.Map.add name data acc)
  in
  if OpamPackage.Name.Map.is_empty results
  then (
    let msg =
      match query with
      | Some q -> Pp.textf "No packages found matching %S." q
      | None -> Pp.text "No packages found."
    in
    User_error.raise [ msg ])
  else (
    let results =
      OpamPackage.Name.Map.bindings results
      |> List.sort ~compare:(fun (a, _) (b, _) ->
        (* Sort exact matches before substring matches *)
        let order =
          match query with
          | Some q ->
            let a_str = OpamPackage.Name.to_string a in
            let b_str = OpamPackage.Name.to_string b in
            if String.equal a_str q
            then -1
            else if String.equal b_str q
            then 1
            else OpamPackage.Name.compare a b
          | None -> OpamPackage.Name.compare a b
        in
        Ordering.of_int order)
    in
    [ Pp.enumerate results ~f:(fun (name, (version, resolved_pkg)) ->
        let synopsis =
          resolved_pkg |> Resolved_package.opam_file |> OpamFile.OPAM.synopsis
        in
        Pp.concat
          ~sep:Pp.space
          [ Pp.tag User_message.Style.Kwd (Pp.verbatim (OpamPackage.Name.to_string name))
          ; Pp.verbatim (OpamPackage.Version.to_string version)
            |> Pp.tag User_message.Style.Hint
          ; Option.value ~default:"(no synopsis)" synopsis
            |> Pp.text
            |> Pp.tag User_message.Style.Details
          ])
    ]
    |> Console.print
    |> Fiber.return)
;;

let term =
  let+ builder = Common.Builder.term
  (* CR-someday Alizter: document this option *)
  and+ query = Arg.(value & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc:None) in
  let builder = Common.Builder.forbid_builds builder in
  let common, config = Common.init builder in
  Scheduler_setup.go_with_rpc_server ~common ~config (fun () ->
    let open Fiber.O in
    Pkg_common.check_pkg_management_enabled () >>> search_packages ~query ())
;;

let info =
  let doc = "Search for packages through repositories in workspace." in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        "Searches through all the repositories configured in the default context's \
         workdir and prints the names of packages that match the given QUERY. QUERY can \
         also be a glob pattern as documented and used in the dependency specifications."
    ; `S "EXAMPLES"
    ; `Pre "  dune pkg search dune"
    ]
  in
  Cmd.info "search" ~doc ~man
;;

let command = Cmd.v info term
