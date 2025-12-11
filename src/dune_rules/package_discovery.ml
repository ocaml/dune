open Import
open Memo.O

type t =
  { package_of_lib : Package.Name.t Lib_name.Map.t
  ; libs_of_package : Lib.t list Package.Name.Map.t
  ; mlds_of_package : Path.t list Package.Name.Map.t
  ; config_of_package : Odoc_config.t Package.Name.Map.t
  ; installed_files : string list Package.Name.Map.t (* Files from .changes files *)
  ; opam_prefix : Path.t option (* Root of opam installation *)
  }

let empty =
  { package_of_lib = Lib_name.Map.empty
  ; libs_of_package = Package.Name.Map.empty
  ; mlds_of_package = Package.Name.Map.empty
  ; config_of_package = Package.Name.Map.empty
  ; installed_files = Package.Name.Map.empty
  ; opam_prefix = None
  }
;;

(* Parse an opam changes file using the opam-format library *)
let parse_changes_file ~changes_file_path contents =
  let filename =
    OpamFile.make (OpamFilename.of_string (Path.to_string changes_file_path))
  in
  try
    let changed =
      (* Remove opam-version line if present, as it's invalid in changes files *)
      let clean_contents =
        if String.is_prefix contents ~prefix:"opam-version"
        then (
          match String.index contents '\n' with
          | Some pos ->
            let len = String.length contents in
            String.sub contents ~pos:(pos + 1) ~len:(len - pos - 1)
          | None -> "")
        else contents
      in
      OpamFile.Changes.read_from_string ~filename clean_contents
    in
    let added =
      OpamStd.String.Map.fold
        (fun file track acc ->
           match track with
           | OpamDirTrack.Added _ | OpamDirTrack.Contents_changed _ -> file :: acc
           | _ -> acc)
        changed
        []
    in
    added
  with
  | OpamPp.Bad_version _ | OpamPp.Bad_format _ ->
    (* Failed to parse, return empty list *)
    []
;;

let read_opam_changes_file ~opam_prefix ~package_name =
  let changes_file =
    Path.relative
      opam_prefix
      (sprintf ".opam-switch/install/%s.changes" (Package.Name.to_string package_name))
  in
  (* Convert to Outside_build_dir.t as required by Fs_memo *)
  let changes_file_external = Path.as_outside_build_dir_exn changes_file in
  let* exists = Fs_memo.file_exists changes_file_external in
  if exists
  then
    let+ contents = Fs_memo.file_contents changes_file_external in
    let files = parse_changes_file ~changes_file_path:changes_file contents in
    Some files
  else
    (* Changes file doesn't exist *)
    Memo.return None
;;

let discover_opam_packages ~opam_prefix =
  (* List all *.changes files in .opam-switch/install/ *)
  let install_dir = Path.relative opam_prefix ".opam-switch/install" in
  let install_dir_external = Path.as_outside_build_dir_exn install_dir in
  let* dir_result = Fs_memo.dir_contents ~force_update:false install_dir_external in
  match dir_result with
  | Error _ ->
    (* Directory doesn't exist *)
    Memo.return []
  | Ok contents ->
    let changes_files =
      Fs_cache.Dir_contents.to_list contents
      |> List.filter_map ~f:(fun (filename, _kind) ->
        if String.is_suffix filename ~suffix:".changes"
        then (
          match String.drop_suffix filename ~suffix:".changes" with
          | Some pkg_name -> Some (Package.Name.of_string pkg_name)
          | None -> None)
        else None)
    in
    let+ packages_with_files =
      Memo.parallel_map changes_files ~f:(fun pkg_name ->
        let+ files = read_opam_changes_file ~opam_prefix ~package_name:pkg_name in
        pkg_name, files)
    in
    List.filter_map packages_with_files ~f:(fun (pkg_name, files) ->
      match files with
      | None -> None
      | Some files -> Some (pkg_name, files))
;;

let find_package_for_library ~file_to_package_map lib =
  let lib_info = Lib.info lib in
  let lib_name = Lib.name lib in
  (* Get the actual archive files for this library *)
  let archives = Lib_info.archives lib_info in
  let byte_archives = Mode.Dict.get archives Byte in
  let native_archives = Mode.Dict.get archives Native in
  let all_archives = byte_archives @ native_archives in
  (* Special case for stdlib which has no archives - construct the expected path *)
  let all_archives =
    if List.is_empty all_archives && Lib_name.equal lib_name (Lib_name.of_string "stdlib")
    then (
      (* stdlib archive should be at lib/ocaml/stdlib.cma relative to opam prefix *)
      let src_dir = Lib_info.src_dir lib_info in
      [ Path.relative src_dir "stdlib.cma" ])
    else all_archives
  in
  (* Look up each archive file in the opam changes map to see which package installed it *)
  let pkg_from_archives =
    List.find_map all_archives ~f:(fun archive_path ->
      Path.Map.find file_to_package_map archive_path)
  in
  match pkg_from_archives with
  | Some pkg -> Some pkg
  | None ->
    (* No archives found (e.g., dummy packages like "bytes").
       Fallback: check if the META file is in the installed files. *)
    let src_dir = Lib_info.src_dir lib_info in
    let meta_path = Path.relative src_dir "META" in
    Path.Map.find file_to_package_map meta_path
;;

let build_mappings_from_changes_data ~file_to_package_map libs =
  List.fold_left libs ~init:empty ~f:(fun acc lib ->
    match find_package_for_library ~file_to_package_map lib with
    | None -> acc
    | Some pkg_name ->
      let lib_name = Lib.name lib in
      { acc with
        package_of_lib = Lib_name.Map.set acc.package_of_lib lib_name pkg_name
      ; libs_of_package =
          Package.Name.Map.update acc.libs_of_package pkg_name ~f:(function
            | None -> Some [ lib ]
            | Some libs -> Some (lib :: libs))
      })
;;

(* Build all package maps in a single pass over packages_with_files *)
let build_package_maps packages_with_files ~opam_prefix =
  List.fold_left
    packages_with_files
    ~init:
      ( Path.Map.empty (* file_to_package *)
      , Package.Name.Map.empty (* installed_files *)
      , Package.Name.Map.empty (* mlds *)
      , Package.Name.Map.empty (* configs *) )
    ~f:(fun (file_to_pkg, installed, mlds, configs) (pkg_name, files) ->
      let pkg_str = Package.Name.to_string pkg_name in
      (* Build file_to_package map *)
      let file_to_pkg =
        List.fold_left files ~init:file_to_pkg ~f:(fun acc file_str ->
          Path.Map.set acc (Path.relative opam_prefix file_str) pkg_name)
      in
      (* Build installed_files map *)
      let installed = Package.Name.Map.set installed pkg_name files in
      (* Extract mld files: doc/{package}/odoc-pages/**/*.mld *)
      let mld_files =
        List.filter_map files ~f:(fun file_str ->
          match String.split file_str ~on:'/' with
          | "doc" :: pkg :: "odoc-pages" :: _ when String.equal pkg pkg_str ->
            if String.is_suffix file_str ~suffix:".mld"
            then Some (Path.relative opam_prefix file_str)
            else None
          | _ -> None)
      in
      let mlds =
        if List.is_empty mld_files
        then mlds
        else Package.Name.Map.set mlds pkg_name mld_files
      in
      (* Extract config: doc/{package}/odoc-config.sexp *)
      let configs =
        match
          List.find_opt files ~f:(fun file_str ->
            match String.split file_str ~on:'/' with
            | [ "doc"; pkg; "odoc-config.sexp" ] when String.equal pkg pkg_str -> true
            | _ -> false)
        with
        | None -> configs
        | Some config_file_str ->
          let config = Odoc_config.load (Path.relative opam_prefix config_file_str) in
          Package.Name.Map.set configs pkg_name config
      in
      file_to_pkg, installed, mlds, configs)
;;

let get_opam_prefix ~context =
  let kind = Context.kind context in
  match kind with
  | Opam { root = _; switch = _ } | Default | Lock _ ->
    (* Get opam prefix from environment variable for all context types *)
    let* env = Context.installed_env context in
    (match Env.get env Opam_switch.opam_switch_prefix_var_name with
     | Some prefix -> Memo.return (Some (Path.of_string prefix))
     | None -> Memo.return None)
;;

let create_impl context =
  let* opam_prefix = get_opam_prefix ~context in
  match opam_prefix with
  | None -> Memo.return empty
  | Some prefix ->
    let* packages_with_files = discover_opam_packages ~opam_prefix:prefix in
    let file_to_package, installed_files_map, mlds_map, config_map =
      build_package_maps packages_with_files ~opam_prefix:prefix
    in
    (* Map all installed libraries to their packages via .changes files *)
    let* installed_libs = Lib.DB.installed context in
    let* all_libs_set = Lib.DB.all installed_libs in
    let all_libs = Lib.Set.to_list all_libs_set in
    Log.info
      [ Pp.textf
          "Package_discovery.create_impl: processing %d libs"
          (List.length all_libs)
      ];
    let lib_mappings =
      build_mappings_from_changes_data ~file_to_package_map:file_to_package all_libs
    in
    let obc_libs =
      Package.Name.Map.find
        lib_mappings.libs_of_package
        (Package.Name.of_string "ocaml-base-compiler")
    in
    Log.info
      [ Pp.textf
          "Package_discovery: ocaml-base-compiler has %d libs"
          (Option.value ~default:[] obc_libs |> List.length)
      ];
    Memo.return
      { lib_mappings with
        mlds_of_package = mlds_map
      ; config_of_package = config_map
      ; installed_files = installed_files_map
      ; opam_prefix = Some prefix
      }
;;

let create =
  let memo = Memo.create "package-discovery" ~input:(module Context) create_impl in
  fun ~context -> Memo.exec memo context
;;

let package_of_library t lib =
  (match Lib.Local.of_lib lib with
   | Some _ ->
     Code_error.raise
       "Package_discovery.package_of_library called on local library"
       [ "lib", Lib_name.to_dyn (Lib.name lib) ]
   | None -> ());
  let lib_name = Lib.name lib in
  Lib_name.Map.find t.package_of_lib lib_name
;;

let libraries_of_package t pkg =
  Package.Name.Map.find t.libs_of_package pkg |> Option.value ~default:[]
;;

let mlds_of_package t pkg =
  Package.Name.Map.find t.mlds_of_package pkg |> Option.value ~default:[]
;;

let module_source_file t ~lib ~module_name =
  let open Option.O in
  let* prefix = t.opam_prefix in
  let* pkg = Lib_name.Map.find t.package_of_lib (Lib.name lib) in
  let* files = Package.Name.Map.find t.installed_files pkg in
  let src_dir = Lib_info.src_dir (Lib.info lib) in
  let module_name_lower = String.uncapitalize_ascii module_name in
  let rel_dir =
    match Path.drop_prefix src_dir ~prefix with
    | Some local -> Path.Local.to_string local
    | None -> Path.to_string src_dir
  in
  List.find_map [ ".cmti"; ".cmt" ] ~f:(fun ext ->
    let rel_file =
      if String.is_empty rel_dir
      then module_name_lower ^ ext
      else rel_dir ^ "/" ^ module_name_lower ^ ext
    in
    if List.mem files rel_file ~equal:String.equal
    then Some (Path.relative src_dir (module_name_lower ^ ext))
    else None)
;;

let config_of_package t pkg =
  Package.Name.Map.find t.config_of_package pkg |> Option.value ~default:Odoc_config.empty
;;

(* Parse switch-state file to extract installed package versions.
   The switch-state file contains an "installed:" field with entries like "pkg.version" *)
let parse_switch_state_installed contents =
  let open OpamParserTypes.FullPos in
  try
    let lexbuf = Lexing.from_string contents in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "switch-state" };
    let opam_file = OpamBaseParser.main OpamLexer.token lexbuf "switch-state" in
    (* Find the "installed" field *)
    let installed_field =
      List.find_map opam_file.file_contents ~f:(fun item ->
        match item.pelem with
        | Variable (var, value) when String.equal var.pelem "installed" -> Some value
        | _ -> None)
    in
    match installed_field with
    | None -> Package.Name.Map.empty
    | Some value ->
      (* Extract list of strings like "pkg.version" *)
      (match value.pelem with
       | List items ->
         List.fold_left items.pelem ~init:Package.Name.Map.empty ~f:(fun acc item ->
           match item.pelem with
           | String pkg_version ->
             (* Parse "pkg.version" - split on first dot *)
             (match String.lsplit2 pkg_version ~on:'.' with
              | Some (name, version) ->
                Package.Name.Map.set acc (Package.Name.of_string name) version
              | None -> acc)
           | _ -> acc)
       | _ -> Package.Name.Map.empty)
  with
  | _ -> Package.Name.Map.empty
;;

(* Get the version of an installed package by reading from switch-state file *)
let version_of_package t pkg =
  match t.opam_prefix with
  | None -> Memo.return None
  | Some prefix ->
    let switch_state_path = Path.relative prefix ".opam-switch/switch-state" in
    let switch_state_external = Path.as_outside_build_dir_exn switch_state_path in
    let* exists = Fs_memo.file_exists switch_state_external in
    if not exists
    then Memo.return None
    else
      let+ contents = Fs_memo.file_contents switch_state_external in
      let installed_versions = parse_switch_state_installed contents in
      Package.Name.Map.find installed_versions pkg
;;
