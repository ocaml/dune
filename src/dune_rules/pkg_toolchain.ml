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
