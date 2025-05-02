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
