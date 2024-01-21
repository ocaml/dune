open Import
open Memo.O

module Clflags = struct
  type t =
    | No_restriction
    | Restrict of
        { names : Package.Name.Set.t
        ; command_line_option : string
        }

  let to_dyn = function
    | No_restriction -> Dyn.Variant ("No_restriction", [])
    | Restrict { names; command_line_option } ->
      Variant
        ( "Restrict"
        , [ Record
              [ "names", Package.Name.Set.to_dyn names
              ; "command_line_option", String command_line_option
              ]
          ] )
  ;;

  let t = Fdecl.create to_dyn
  let set x = Fdecl.set t x
  let t () = Fdecl.get t
end

let conf =
  Memo.lazy_ ~name:"only_packages" (fun () ->
    match Clflags.t () with
    | No_restriction -> Memo.return None
    | Restrict { names; command_line_option } ->
      let* packages = Dune_load.load () >>| Dune_load.packages in
      Package.Name.Set.iter names ~f:(fun pkg_name ->
        if not (Package.Name.Map.mem packages pkg_name)
        then (
          let pkg_name = Package.Name.to_string pkg_name in
          User_error.raise
            [ Pp.textf
                "I don't know about package %s (passed through %s)"
                pkg_name
                command_line_option
            ]
            ~hints:
              (User_message.did_you_mean
                 pkg_name
                 ~candidates:
                   (Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string))));
      Package.Name.Map.to_list packages
      |> Memo.parallel_map ~f:(fun (name, pkg) ->
        let+ vendored = Source_tree.is_vendored (Package.dir pkg) in
        let included = Package.Name.Set.mem names name in
        if vendored && included
        then
          User_error.raise
            [ Pp.textf
                "Package %s is vendored and so will never be masked. It is redundant to \
                 pass it to %s."
                (Package.Name.to_string name)
                command_line_option
            ];
        Option.some_if (vendored || included) (name, pkg))
      >>| List.filter_opt
      >>| Package.Name.Map.of_list_exn
      >>| Option.some)
;;

let get_mask () = Memo.Lazy.force conf

let packages_of_project project =
  let+ t = Memo.Lazy.force conf in
  let packages = Dune_project.packages project in
  match t with
  | None -> packages
  | Some mask ->
    Package.Name.Map.merge packages mask ~f:(fun _ p mask ->
      match p, mask with
      | Some _, Some _ -> p
      | _ -> None)
;;

let filter_out_stanzas_from_hidden_packages ~visible_pkgs =
  List.filter_map ~f:(fun stanza ->
    let include_stanza =
      match Dune_file.stanza_package stanza with
      | None -> true
      | Some package ->
        let name = Package.name package in
        Package.Name.Map.mem visible_pkgs name
    in
    if include_stanza
    then Some stanza
    else (
      match Stanza.repr stanza with
      | Library.T l ->
        let open Option.O in
        let+ redirect = Dune_file.Library_redirect.Local.of_private_lib l in
        Dune_file.Library_redirect.Local.make_stanza redirect
      | _ -> None))
;;

type filtered_stanzas =
  { all : Dune_file.t list
  ; map : Dune_file.t Path.Source.Map.t
  }

let filtered_stanzas =
  let db =
    Per_context.create_by_name ~name:"filtered_stanzas"
    @@ fun context ->
    Memo.lazy_ (fun () ->
      let+ only_packages = Memo.Lazy.force conf
      and+ stanzas =
        Dune_load.load () >>| Dune_load.dune_files >>= Dune_load.Dune_files.eval ~context
      in
      let all =
        match only_packages with
        | None -> stanzas
        | Some visible_pkgs ->
          List.map stanzas ~f:(fun (dune_file : Dune_file.t) ->
            { dune_file with
              stanzas =
                filter_out_stanzas_from_hidden_packages ~visible_pkgs dune_file.stanzas
            })
      in
      let map =
        Path.Source.Map.of_list_map_exn all ~f:(fun (dune_file : Dune_file.t) ->
          dune_file.dir, dune_file)
      in
      { all; map })
    |> Memo.Lazy.force
  in
  fun ctx -> Staged.unstage db ctx
;;

let get () =
  let* packages = Dune_load.load () >>| Dune_load.packages in
  let+ only_packages = Memo.Lazy.force conf in
  Option.value only_packages ~default:packages
;;

let stanzas_in_dir dir =
  if Path.Build.is_root dir
  then Memo.return None
  else (
    match Install.Context.of_path dir with
    | None -> Memo.return None
    | Some ctx ->
      let+ filtered_stanzas = filtered_stanzas ctx in
      let dir = Path.Build.drop_build_context_exn dir in
      Path.Source.Map.find filtered_stanzas.map dir)
;;

let filtered_stanzas ctx =
  let+ filtered_stanzas = filtered_stanzas ctx in
  filtered_stanzas.all
;;
