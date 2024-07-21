open Import

let base_dir () =
  let cache_dir =
    Lazy.force Dune_util.xdg |> Xdg.cache_dir |> Path.Outside_build_dir.of_string
  in
  let path =
    Path.Outside_build_dir.relative
      (Path.Outside_build_dir.relative cache_dir "dune")
      "toolchains"
  in
  (let path = Path.outside_build_dir path in
   if not (Path.Untracked.exists path) then Path.mkdir_p path;
   if not (Path.Untracked.is_directory path)
   then
     User_error.raise
       [ Pp.textf "Expected %s to be a directory but it is not." (Path.to_string path) ]);
  path
;;

let make_bool ~name ~default =
  let of_string s =
    match Bool.of_string s with
    | Some b -> Ok b
    | None ->
      Error (sprintf "%s is not a bool (must be \"true\" or \"false\")" (String.quoted s))
  in
  Config.make ~name ~of_string ~default
;;

let enabled = make_bool ~name:"toolchains_enabled" ~default:false

(** When building the compiler with toolchains, build it with [make
      -j] rather than [make -j1], allowing more parallelism. This can
    theoretically lead to build failures, but these are extremely rare in
    practice. *)
let build_compiler_in_parallel =
  make_bool ~name:"toolchains_build_compiler_in_parallel" ~default:false
;;

let is_compiler_and_toolchains_enabled name =
  Config.get enabled
  &&
  let module Package_name = Dune_pkg.Package_name in
  let compiler_package_names =
    Package_name.Set.of_list (* TODO don't hardcode these names here *)
      [ Package_name.of_string "ocaml-base-compiler"
      ; Package_name.of_string "ocaml-variants"
      ]
  in
  Package_name.Set.mem compiler_package_names name
;;

let files ~bin_dir =
  let open Memo.O in
  Fs_memo.dir_contents bin_dir
  >>| function
  | Error _ -> Section.Map.empty
  | Ok files ->
    let bin_paths =
      Fs_cache.Dir_contents.to_list files
      |> List.filter_map ~f:(fun (filename, kind) ->
        match kind with
        | Unix.S_REG | S_LNK ->
          let path = Path.Outside_build_dir.relative bin_dir filename in
          (try
             Unix.access (Path.Outside_build_dir.to_string path) [ Unix.X_OK ];
             Some (Path.outside_build_dir path)
           with
           | Unix.Unix_error _ -> None)
        | _ -> None)
    in
    Section.Map.singleton Section.Bin bin_paths
;;

let ocaml context env ~bin_dir =
  let which prog =
    let open Memo.O in
    let path = Path.Outside_build_dir.relative bin_dir prog in
    let+ exists = Fs_memo.file_exists path in
    if exists then Some (Path.outside_build_dir path) else None
  in
  let get_ocaml_tool ~dir:_ prog = which prog in
  Ocaml_toolchain.make context ~which ~env ~get_ocaml_tool
;;

let dummy_fetch ~target name version ~installation_prefix =
  Action.progn
    [ Action.mkdir target
    ; Action.with_stdout_to
        (Path.Build.relative target "debug_hint.txt")
        (Action.echo
           [ sprintf
               "This file was created as a hint to people debugging issues with dune \
                package management.\n\
                The current project attempted to download and install the package %s.%s, \
                however a matching package was found in the toolchains directory %s, so \
                the latter will be used instead."
               (Dune_pkg.Package_name.to_string name)
               (Dune_pkg.Package_version.to_string version)
               (installation_prefix |> Path.Outside_build_dir.to_string)
           ])
    ; (* TODO: it doesn't seem like it should be necessary to generate
         this file but without it dune complains *)
      Action.with_stdout_to
        (Path.Build.relative target "config.cache")
        (Action.echo
           [ "Dummy file created to placate dune's package installation rules. See the \
              debug_hint.txt file for more information."
           ])
    ]
  |> Action.Full.make
  |> Action_builder.With_targets.return
  |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

(* The path to the directory containing the artifacts within the
   temporary install directory. When installing with the DESTDIR
   variable, the absolute path to the final installation directory is
   concatenated to the value of DESTDIR. *)
let installation_prefix_within_tmp_install_dir ~installation_prefix:prefix tmp_install_dir
  =
  let target_without_root_prefix =
    (* Remove the root directory prefix from the target directory so
       it can be used to create a path relative to the temporary
       install dir. *)
    match
      String.drop_prefix
        (Path.Outside_build_dir.to_string prefix)
        ~prefix:(Path.External.to_string Path.External.root)
    with
    | Some x -> x
    | None ->
      Code_error.raise
        "Expected prefix to start with root"
        [ "prefix", Path.Outside_build_dir.to_dyn prefix
        ; "root", Path.External.to_dyn Path.External.root
        ]
  in
  Path.relative tmp_install_dir target_without_root_prefix
;;

let modify_install_action action ~installation_prefix ~suffix =
  match action with
  | Dune_lang.Action.Run [ Literal make; Literal install ] ->
    (match String_with_vars.pform_only make, String_with_vars.text_only install with
     | Some (Pform.Var Pform.Var.Make), Some "install" ->
       let tmp_install_dir = Temp.create Dir ~prefix:"dune-toolchain-destdir" ~suffix in
       let action =
         (* Set the DESTDIR variable so installed artifacts are not immediately
            placed in the final installation directory. *)
         Dune_lang.Action.Run
           [ Literal make
           ; Literal install
           ; Slang.text (sprintf "DESTDIR=%s" (Path.to_string tmp_install_dir))
           ]
       in
       let prefix = Path.outside_build_dir installation_prefix in
       (* Append some commands to the install command that copy
          the artifacts to their final installation directory. *)
       Dune_lang.Action.Progn
         [ action
         ; Dune_lang.Action.Run
             [ Slang.text "mkdir"
             ; Slang.text "-p"
             ; Slang.text @@ Path.to_string @@ Path.parent_exn prefix
             ]
         ; Dune_lang.Action.Run
             [ Slang.text "mv"
             ; (* Prevents mv from replacing the destination if it
                  already exists. This can happen if two dune
                  instances race to install the toolchain. Note
                  that -n is not posix but it is supported by gnu
                  coreutils and by the default mv command on
                  macos, but not openbsd. *)
               Slang.text "-n"
             ; Slang.text
                 (Path.to_string
                  @@ installation_prefix_within_tmp_install_dir
                       ~installation_prefix
                       tmp_install_dir)
             ; Slang.text @@ Path.to_string @@ Path.parent_exn prefix
             ]
         ]
     | _ ->
       (* The install command is something other than `make install`, so don't
          attempt to modify. *)
       action)
  | _ ->
    (* Not a "run" action, so don't attempt to modify. *)
    action
;;

module Override_pform = struct
  type t =
    { prefix : Path.t option
    ; doc : Path.t option
    ; jobs : string option
    }

  let empty = { prefix = None; doc = None; jobs = None }

  let make ~installation_prefix =
    let prefix = Path.outside_build_dir installation_prefix in
    { prefix = Some prefix
    ; doc = Some (Path.relative prefix "doc")
    ; jobs =
        (if Config.get build_compiler_in_parallel
         then (* build with more parallelism (i.e. `make -j`) *)
           Some ""
         else None)
    }
  ;;
end
