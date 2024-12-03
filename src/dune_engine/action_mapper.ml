open Import

module Make (Src : Action_intf.Ast) (Dst : Action_intf.Ast) = struct
  type map =
    Src.t
    -> dir:Src.path
    -> f_program:(dir:Src.path -> Src.program -> Dst.program)
    -> f_string:(dir:Src.path -> Src.string -> Dst.string)
    -> f_path:(dir:Src.path -> Src.path -> Dst.path)
    -> f_target:(dir:Src.path -> Src.target -> Dst.target)
    -> f_ext:(dir:Src.path -> Src.ext -> Dst.ext)
    -> Dst.t

  let map_one_step f (t : Src.t) ~dir ~f_program ~f_string ~f_path ~f_target ~f_ext
    : Dst.t
    =
    let f t ~dir = f t ~dir ~f_program ~f_string ~f_path ~f_target ~f_ext in
    match t with
    | Run (prog, args) ->
      Run (f_program ~dir prog, Array.Immutable.map args ~f:(f_string ~dir))
    | With_accepted_exit_codes (pred, t) -> With_accepted_exit_codes (pred, f t ~dir)
    | Dynamic_run (prog, args) ->
      Dynamic_run (f_program ~dir prog, List.map args ~f:(f_string ~dir))
    | Chdir (fn, t) -> Chdir (f_path ~dir fn, f t ~dir:fn)
    | Setenv (var, value, t) -> Setenv (f_string ~dir var, f_string ~dir value, f t ~dir)
    | Redirect_out (outputs, fn, perm, t) ->
      Redirect_out (outputs, f_target ~dir fn, perm, f t ~dir)
    | Redirect_in (inputs, fn, t) -> Redirect_in (inputs, f_path ~dir fn, f t ~dir)
    | Ignore (outputs, t) -> Ignore (outputs, f t ~dir)
    | Progn l -> Progn (List.map l ~f:(fun t -> f t ~dir))
    | Concurrent l -> Concurrent (List.map l ~f:(fun t -> f t ~dir))
    | Echo xs -> Echo (List.map xs ~f:(f_string ~dir))
    | Cat xs -> Cat (List.map xs ~f:(f_path ~dir))
    | Copy (x, y) -> Copy (f_path ~dir x, f_target ~dir y)
    | Symlink (x, y) -> Symlink (f_path ~dir x, f_target ~dir y)
    | Hardlink (x, y) -> Hardlink (f_path ~dir x, f_target ~dir y)
    | Bash x -> Bash (f_string ~dir x)
    | Write_file (x, perm, y) -> Write_file (f_target ~dir x, perm, f_string ~dir y)
    | Rename (x, y) -> Rename (f_target ~dir x, f_target ~dir y)
    | Remove_tree x -> Remove_tree (f_target ~dir x)
    | Mkdir x -> Mkdir (f_target ~dir x)
    | Pipe (outputs, l) -> Pipe (outputs, List.map l ~f:(fun t -> f t ~dir))
    | Extension ext -> Extension (f_ext ~dir ext)
    | Needed_deps xs -> Needed_deps (List.map xs ~f:(f_path ~dir))
  ;;

  let rec map t ~dir ~f_program ~f_string ~f_path ~f_target ~f_ext =
    map_one_step map t ~dir ~f_program ~f_string ~f_path ~f_target ~f_ext
  ;;
end
