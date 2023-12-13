open Import

let expand_version loc macro =
  Expander0.Expansion_result.Need_full_expander
    (fun expander ->
      let version =
        let s = Pform.Macro_invocation.Args.whole macro in
        let open Memo.O in
        let* scope = Scope.DB.find_by_dir (Expander0.dir expander) in
        let value_from_version = function
          | None -> [ Value.String "" ]
          | Some s -> [ String (Package_version.to_string s) ]
        in
        let project = Scope.project scope in
        match
          Package.Name.Map.find (Dune_project.packages project) (Package.Name.of_string s)
        with
        | Some p -> Memo.return (value_from_version p.version)
        | None when Dune_project.dune_version project < (2, 9) ->
          User_error.raise
            ~loc
            [ Pp.textf "Package %S doesn't exist in the current project." s ]
            ~hints:
              [ Pp.text
                  "If you want to refer to an installed package, or more generally to a \
                   package from another project, you need at least (lang dune 2.9)."
              ]
        | None ->
          let libname = Lib_name.of_string s in
          let pkgname = Lib_name.package_name libname in
          if not (String.equal (Package.Name.to_string pkgname) s)
          then
            User_error.raise
              ~loc
              [ Pp.textf
                  "Library names are not allowed in this position. Only package names \
                   are allowed"
              ];
          let open Memo.O in
          Lib.DB.find (Scope.libs scope) libname
          >>| (function
           | Some lib -> value_from_version (Lib_info.version (Lib.info lib))
           | None ->
             User_error.raise
               ~loc
               [ Pp.textf
                   "Package %S doesn't exist in the current project and isn't installed \
                    either."
                   s
               ])
      in
      Without version)
;;

let lib_available loc macro =
  Expander0.Expansion_result.Need_full_expander
    (fun expander ->
      Without
        (let dir = Expander0.dir expander in
         let lib =
           let s = Pform.Macro_invocation.Args.whole macro in
           Lib_name.parse_string_exn (loc, s)
         in
         let open Memo.O in
         let+ available =
           let* scope = Scope.DB.find_by_dir dir in
           Lib.DB.available (Scope.libs scope) lib
         in
         [ Value.String (string_of_bool available) ]))
;;

let file_of_lib db context ~loc ~lib ~file =
  let open Resolve.Memo.O in
  let+ lib = Lib.DB.resolve db (loc, lib) in
  let dir =
    let info = Lib.info lib in
    match Lib.is_local lib with
    | false -> Lib_info.src_dir info
    | true ->
      let name = Lib.name lib in
      let subdir =
        Lib_info.Status.relative_to_package (Lib_info.status info) name
        |> Option.value_exn
      in
      let pkg_root =
        let package = Lib_name.package_name name in
        (* Why do we return the install path? *)
        Install.Context.lib_dir ~context ~package
      in
      Path.build (Path.Build.append_local pkg_root subdir)
  in
  Path.relative dir file
;;

let expand_lib_variable loc macro ctx ~dir ~lib_exec ~lib_private =
  let context = Context.DB.get ctx in
  let context_host = Memo.bind context ~f:Context.host in
  let open Action_builder.O in
  let* scope = Action_builder.of_memo @@ Scope.DB.find_by_dir dir in
  let project = Scope.project scope in
  let* scope_host =
    Action_builder.of_memo
    @@
    let open Memo.O in
    let* context = context in
    let* host = context_host in
    if Context.equal context host
    then Memo.return scope
    else (
      let scope_dir =
        let without_ctx = Path.Build.drop_build_context_exn dir in
        Path.Build.append_source (Context.build_dir host) without_ctx
      in
      Scope.DB.find_by_dir scope_dir)
  in
  let scope = if lib_exec then scope else scope_host in
  let lib, file = Pform.Macro_invocation.Args.lsplit2_exn macro loc in
  let lib = Lib_name.parse_string_exn (loc, lib) in
  let p =
    let open Resolve.Memo.O in
    if lib_private
    then
      let* lib = Lib.DB.resolve (Scope.libs scope) (loc, lib) in
      let referenced_project =
        Lib.info lib |> Lib_info.status |> Lib_info.Status.project
      in
      if Option.equal Dune_project.equal (Some project) referenced_project
      then Resolve.Memo.return (Path.relative (Lib_info.src_dir (Lib.info lib)) file)
      else
        Resolve.Memo.fail
          (User_error.make
             ~loc
             [ Pp.textf
                 "The variable \"lib%s-private\" can only refer to libraries within the \
                  same project. The current project's name is %S, but the reference is \
                  to %s."
                 (if lib_exec then "exec" else "")
                 (Dune_project.Name.to_string_hum (Dune_project.name project))
                 (match referenced_project with
                  | None -> "an external library"
                  | Some project ->
                    Dune_project.name project
                    |> Dune_project.Name.to_string_hum
                    |> String.quoted)
             ])
    else
      let* context = Resolve.Memo.lift_memo context in
      let* lib_artifacts = Resolve.Memo.lift_memo @@ Scope.DB.public_libs context in
      file_of_lib lib_artifacts (Context.name context) ~loc ~lib ~file
  in
  let p =
    let open Memo.O in
    Resolve.Memo.peek p
    >>| function
    | Ok p ->
      (match file with
       | "" | "." ->
         let lang_version = Dune_project.dune_version project in
         if lang_version < (3, 0)
         then Action_builder.return [ Value.Path p ]
         else
           User_error.raise
             ~loc
             [ Pp.textf
                 "The form %%{%s:<libname>:%s} is no longer supported since version 3.0 \
                  of the Dune language."
                 (if lib_private then "lib-private" else "lib")
                 file
             ]
             ~hints:
               [ (match Lib_name.to_string lib with
                  | "ctypes" ->
                    Pp.text
                      "Did you know that Dune 3.0 supports ctypes natively? See the \
                       manual for more details."
                  | _ ->
                    Pp.textf
                      "If you are trying to use this form to include a directory, you \
                       should instead use (foreign_stubs (include_dirs (lib %s))). See \
                       the manual for more details."
                      (Lib_name.to_string lib))
               ]
       | _ ->
         if (not lib_exec) || (not Sys.win32) || Filename.extension file = ".exe"
         then Expander0.Deps.dep p
         else (
           let p_exe = Path.extend_basename p ~suffix:".exe" in
           Action_builder.if_file_exists
             p_exe
             ~then_:(Expander0.Deps.dep p_exe)
             ~else_:(Expander0.Deps.dep p)))
    | Error () ->
      let p =
        if lib_private
        then Resolve.Memo.map p ~f:(fun _ -> assert false)
        else
          let open Resolve.Memo.O in
          let* available =
            Resolve.Memo.lift_memo (Lib.DB.available (Scope.libs scope) lib)
          in
          match available with
          | false ->
            let+ _ = p in
            assert false
          | true ->
            Resolve.Memo.fail
              (User_error.make
                 ~loc
                 [ Pp.textf
                     "The library %S is not public. The variable \"lib%s\" expands to \
                      the file's installation path which is not defined for private \
                      libraries."
                     (Lib_name.to_string lib)
                     (if lib_exec then "exec" else "")
                 ])
      in
      Resolve.Memo.read p
  in
  Action_builder.of_memo_join p
;;

let forms =
  let libs =
    let bools = [ true; false ] in
    List.concat_map bools ~f:(fun y -> List.map bools ~f:(fun x -> x, y))
    |> List.map ~f:(fun (lib_exec, lib_private) ->
      let pform = Pform.Macro.Lib { lib_exec; lib_private } in
      ( pform
      , let f = expand_lib_variable ~lib_private ~lib_exec in
        fun loc macro ->
          Expander0.Expansion_result.Need_full_expander
            (fun expander ->
              let dir = Expander0.dir expander in
              let context = Expander0.context expander in
              With (f loc macro ~dir context)) ))
  in
  libs @ [ Version, expand_version; Lib_available, lib_available ]
  |> Pform.Macro.Map.of_list_exn
;;

let project_root _loc =
  Expander0.Expansion_result.Need_full_expander
    (fun t ->
      let dir = Expander0.dir t in
      Without
        (let open Memo.O in
         let+ scope = Scope.DB.find_by_dir dir in
         [ Value.Dir (Path.build (Scope.root scope)) ]))
;;

let vars = [ Pform.Var.Project_root, project_root ] |> Pform.Var.Map.of_list_exn
let () = Expander0.Source.make vars forms
let linkme = ()
