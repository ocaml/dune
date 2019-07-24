open Stdune

type t =
  { name : string
  ; recursive : bool
  ; dir : Path.Source.t
  ; contexts : Dune.Context.t list
  }

let to_log_string { name ; recursive; dir ; contexts = _ } =
  sprintf "- %salias %s%s/%s"
    (if recursive then "recursive " else "")
    (if recursive then "@" else "@@")
    (Path.Source.to_string_maybe_quoted dir)
    name

let in_dir ~name ~recursive ~contexts dir =
  let checked = Util.check_path contexts dir in
  match checked with
  | External _ ->
    User_error.raise
      [ Pp.textf "@@ on the command line must be followed by a relative path" ]
  | In_source_dir dir ->
    { dir
    ; recursive
    ; name
    ; contexts
    }
  | In_install_dir _ ->
    User_error.raise
      [ Pp.textf "Invalid alias: %s."
          (Path.to_string_maybe_quoted Path.(relative build_dir "install"))
      ; Pp.textf "There are no aliases in %s."
          (Path.to_string_maybe_quoted dir)
      ]
  | In_build_dir (ctx, dir) ->
    { dir
    ; recursive
    ; name
    ; contexts =
        [List.find_exn contexts ~f:(fun c -> Dune.Context.name c = ctx.name)]
    }

let of_string common s ~contexts =
  if not (String.is_prefix s ~prefix:"@") then
    None
  else
    let pos, recursive =
      if String.length s >= 2 && s.[1] = '@' then
        (2, false)
      else
        (1, true)
    in
    let s = String.drop s pos in
    let path = Path.relative Path.root (Common.prefix_target common s) in
    if Path.is_root path then
      User_error.raise
        [ Pp.textf
            "@ on the command line must be followed by a valid alias name"
        ]
    else
      let dir = Path.parent_exn path in
      let name = Path.basename path in
      Some (in_dir ~name ~recursive ~contexts dir)
