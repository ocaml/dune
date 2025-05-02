open Import

let add_mode_suffix mode s =
  match mode with
  | Mode.Select.All -> s
  | Only mode -> String.concat ~sep:"_" [ s; Mode.to_string mode ]
;;

module Archive = struct
  module Name = struct
    include String

    let to_string t = t

    let path ~dir ~mode archive_name =
      let archive_name = add_mode_suffix mode archive_name in
      Path.Build.relative dir archive_name
    ;;

    let decode =
      Dune_lang.Decoder.plain_string (fun ~loc s ->
        match s with
        | "." | ".." ->
          User_error.raise ~loc [ Pp.textf "%S is not a valid archive name." s ]
        | fn when String.exists fn ~f:Path.is_dir_sep ->
          User_error.raise
            ~loc
            [ Pp.textf "Path separators are not allowed in archive names." ]
        | fn -> fn)
    ;;

    let stubs archive_name = archive_name ^ "_stubs"
    let lib_file_prefix = "lib"

    let lib_file archive_name ~dir ~ext_lib ~mode =
      let archive_name = add_mode_suffix mode archive_name in
      Path.Build.relative dir (sprintf "%s%s%s" lib_file_prefix archive_name ext_lib)
    ;;

    let dll_file archive_name ~dir ~ext_dll ~mode =
      let archive_name = add_mode_suffix mode archive_name in
      Path.Build.relative dir (sprintf "dll%s%s" archive_name ext_dll)
    ;;
  end

  (** Archive directories can appear as part of the [(foreign_archives ...)]
      fields. For example, in [(foreign_archives some/dir/lib1 lib2)], the
      archive [some/dir/lib1] has the directory [some/dir], whereas the archive
      [lib2] does not specify the directory and is assumed to be located in [.]. *)
  module Dir = struct
    type t = string
  end

  type t =
    { dir : Dir.t
    ; name : Name.t
    }

  let dir_path ~dir t = Path.Build.relative dir t.dir
  let name ~mode t = add_mode_suffix mode t.name
  let stubs archive_name = { dir = "."; name = Name.stubs archive_name }

  let decode =
    let open Dune_lang.Decoder in
    let+ s = string in
    { dir = Filename.dirname s; name = Filename.basename s }
  ;;

  let lib_file ~archive ~dir ~ext_lib ~mode =
    let dir = dir_path ~dir archive in
    Name.lib_file archive.name ~dir ~ext_lib ~mode
  ;;

  let dll_file ~archive ~dir ~ext_dll ~mode =
    let dir = dir_path ~dir archive in
    Name.dll_file archive.name ~dir ~ext_dll ~mode
  ;;
end

module Source = struct
  type kind =
    | Stubs of Foreign_stubs.t
    | Ctypes of Ctypes_field.t

  (* we store the entire [stubs] record even though [t] only describes an
     individual source file *)
  type t =
    { kind : kind
    ; path : Path.Build.t
    }

  let language t =
    match t.kind with
    | Stubs stubs -> stubs.language
    | Ctypes _ -> `C
  ;;

  let path t = t.path
  let kind t = t.kind

  let mode t =
    match t.kind with
    | Stubs s -> s.mode
    | Ctypes _ -> All
  ;;

  let user_object_name t =
    t.path |> Path.Build.split_extension |> fst |> Path.Build.basename
  ;;

  let object_name t = user_object_name t |> add_mode_suffix (mode t)
  let make kind ~path = { kind; path }
end

module Sources = struct
  type t = (Loc.t * Source.t) String.Map.t

  let to_list_map t ~f = String.Map.to_list_map t ~f
  let make t = t

  let object_files t ~dir ~ext_obj =
    String.Map.to_list_map t ~f:(fun c _ -> Path.Build.relative dir (c ^ ext_obj))
  ;;

  let has_cxx_sources (t : t) =
    String.Map.exists t ~f:(fun (_loc, source) ->
      let language = Source.language source in
      Foreign_language.(equal `Cxx language))
  ;;
end
