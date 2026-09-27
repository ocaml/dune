open Import

type t =
  { project : Loaded_project.t
  ; source_dir : Source_path.t
  ; relative_dir : Path.Local.t
  ; output_dir : Path.Build.t
  }

let create ~project ~source_dir =
  let relative_dir =
    match Source_path.descendant source_dir ~of_:(Loaded_project.source_root project) with
    | Some relative_dir -> relative_dir
    | None ->
      Code_error.raise
        "Loaded directory is outside its project source root"
        [ "source_dir", Source_path.to_dyn source_dir
        ; "project", Loaded_project.to_dyn project
        ]
  in
  let output_dir =
    Path.Build.append_local (Loaded_project.output_root project) relative_dir
  in
  { project; source_dir; relative_dir; output_dir }
;;

let project t = t.project
let source_dir t = t.source_dir
let relative_dir t = t.relative_dir
let output_dir t = t.output_dir

let to_dyn t =
  Dyn.record
    [ "project", Loaded_project.to_dyn t.project
    ; "source_dir", Source_path.to_dyn t.source_dir
    ; "relative_dir", Path.Local.to_dyn t.relative_dir
    ; "output_dir", Path.Build.to_dyn t.output_dir
    ]
;;
