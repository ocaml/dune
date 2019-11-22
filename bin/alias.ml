open Stdune

type t =
  { name : Dune.Alias.Name.t
  ; recursive : bool
  ; dir : Path.Source.t
  ; contexts : Dune.Context.t list
  }

let to_log_string { name; recursive; dir; contexts = _ } =
  sprintf "- %salias %s%s/%s"
    ( if recursive then
      "recursive "
    else
      "" )
    ( if recursive then
      "@"
    else
      "@@" )
    (Path.Source.to_string_maybe_quoted dir)
    (Dune.Alias.Name.to_string name)

let in_dir ~name ~recursive ~contexts dir =
  let checked = Util.check_path contexts dir in
  match checked with
  | External _ ->
    User_error.raise
      [ Pp.textf "@@ on the command line must be followed by a relative path" ]
  | In_source_dir dir -> { dir; recursive; name; contexts }
  | In_install_dir _ ->
    User_error.raise
      [ Pp.textf "Invalid alias: %s."
          (Path.to_string_maybe_quoted
             (Path.build Dune.Dpath.Build.install_dir))
      ; Pp.textf "There are no aliases in %s." (Path.to_string_maybe_quoted dir)
      ]
  | In_build_dir (ctx, dir) ->
    { dir
    ; recursive
    ; name
    ; contexts =
        [ List.find_exn contexts ~f:(fun c ->
              Dune.Context_name.equal (Dune.Context.name c) ctx.name)
        ]
    }

let of_string common ~recursive s ~contexts =
  let path = Path.relative Path.root (Common.prefix_target common s) in
  if Path.is_root path then
    User_error.raise
      [ Pp.textf "@ on the command line must be followed by a valid alias name"
      ]
  else
    let dir = Path.parent_exn path in
    let name = Dune.Alias.Name.of_string (Path.basename path) in
    in_dir ~name ~recursive ~contexts dir
