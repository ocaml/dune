open Import

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

  let add_mode_suffix mode s =
    match mode with
    | Mode.Select.All -> s
    | Only mode -> String.concat ~sep:"_" [ s; Mode.to_string mode ]
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
