open Import

let base_dir () =
  let cache_dir =
    Lazy.force Dune_cache_storage.Layout.root_dir |> Path.as_outside_build_dir_exn
  in
  let path = Path.Outside_build_dir.relative cache_dir "toolchains" in
  (let path = Path.outside_build_dir path in
   if not (Path.Untracked.exists path) then Path.mkdir_p path;
   if not (Path.Untracked.is_directory path)
   then
     User_error.raise
       [ Pp.textf "Expected %s to be a directory but it is not." (Path.to_string path) ]);
  path
;;

let pkg_dir (pkg : Dune_pkg.Lock_dir.Pkg.t) =
  (* The name of this package's directory within the toolchains
     directory. Includes a hash of some of the package's fields so that
     if a user modifies a package's lockfile in one project, then the
     modified package won't be used in other projects (unless the
     corresponding lockfile in those projects is modified in the same
     way). *)
  let dir_name =
    (* TODO should include resolved deps *)
    let pkg_digest =
      Dune_digest.Feed.compute_digest
        Lock_dir.Pkg.digest_feed
        (Lock_dir.Pkg.remove_locs pkg)
    in
    (* A hash of the fields of a package that affect its installed artifacts *)
    sprintf
      "%s.%s-%s"
      (Package.Name.to_string pkg.info.name)
      (Package_version.to_string pkg.info.version)
      (Dune_digest.to_string pkg_digest)
  in
  Path.Outside_build_dir.relative (base_dir ()) dir_name
;;

let installation_prefix pkg =
  let pkg_dir = pkg_dir pkg in
  Path.Outside_build_dir.relative pkg_dir "target"
;;

let is_compiler_and_toolchains_enabled name =
  match Config.get Compile_time.toolchains with
  | `Enabled ->
    let module Package_name = Dune_pkg.Package_name in
    let compiler_package_names =
      (* TODO don't hardcode these names here *)
      [ Package_name.of_string "ocaml-base-compiler"
      ; Package_name.of_string "ocaml-variants"
      ; Package_name.of_string "ocaml-compiler"
        (* The [ocaml-compiler] package is required to include all the
           packages that might install a compiler, starting from ocaml.5.3.0.
        *)
      ]
    in
    List.mem compiler_package_names name ~equal:Package_name.equal
  | `Disabled -> false
;;

let install_roots ~prefix =
  Install.Roots.make prefix ~relative:Path.Outside_build_dir.relative
;;
