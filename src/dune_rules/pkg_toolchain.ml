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
