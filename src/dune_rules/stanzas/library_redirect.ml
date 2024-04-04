open Import

type 'old_name t =
  { project : Dune_project.t
  ; loc : Loc.t
  ; old_name : 'old_name
  ; new_public_name : Loc.t * Lib_name.t
  }

module Local = struct
  type info =
    { lib_name : Loc.t * Lib_name.Local.t
    ; enabled : Blang.t
    }

  type nonrec t = info t

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let for_lib (lib : Library.t) ~new_public_name ~loc : t =
    let old_name =
      let lib_name = lib.name
      and enabled = lib.enabled_if in
      { lib_name; enabled }
    in
    { loc; new_public_name; old_name; project = lib.project }
  ;;

  let of_private_lib (lib : Library.t) : t option =
    match lib.visibility with
    | Public _ | Private None -> None
    | Private (Some package) ->
      let loc, name = lib.name in
      let package_name = Package.name package in
      let new_public_name = loc, Lib_name.mangled package_name name in
      Some (for_lib lib ~loc ~new_public_name)
  ;;

  let of_lib (lib : Library.t) : t option =
    let open Option.O in
    let* public_name =
      match lib.visibility with
      | Public plib -> Some plib.name
      | Private _ -> None
    in
    if Lib_name.equal (Lib_name.of_local lib.name) (snd public_name)
    then None
    else (
      let loc = fst public_name in
      Some (for_lib lib ~loc ~new_public_name:public_name))
  ;;

  let to_lib_id ~src_dir t =
    let loc = t.loc in
    Lib_id.Local.make ~loc ~src_dir (Lib_name.of_local t.old_name.lib_name)
  ;;
end
