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
              [ ("names", Package.Name.Set.to_dyn names)
              ; ("command_line_option", String command_line_option)
              ]
          ] )

  let t = Fdecl.create to_dyn

  let set x = Fdecl.set t x

  let t () = Fdecl.get t
end

let conf =
  Memo.lazy_ (fun () ->
      match Clflags.t () with
      | No_restriction -> Memo.return None
      | Restrict { names; command_line_option } ->
        let* conf = Dune_load.load () in
        Package.Name.Set.iter names ~f:(fun pkg_name ->
            if not (Package.Name.Map.mem conf.packages pkg_name) then
              let pkg_name = Package.Name.to_string pkg_name in
              User_error.raise
                [ Pp.textf "I don't know about package %s (passed through %s)"
                    pkg_name command_line_option
                ]
                ~hints:
                  (User_message.did_you_mean pkg_name
                     ~candidates:
                       (Package.Name.Map.keys conf.packages
                       |> List.map ~f:Package.Name.to_string)));
        Memo.sequential_map (Package.Name.Map.to_list conf.packages)
          ~f:(fun (name, pkg) ->
            let+ vendored =
              Dune_engine.Source_tree.is_vendored (Package.dir pkg)
            in
            let included = Package.Name.Set.mem names name in
            if vendored && included then
              User_error.raise
                [ Pp.textf
                    "Package %s is vendored and so will never be masked. It is \
                     redundant to pass it to %s."
                    (Package.Name.to_string name)
                    command_line_option
                ];
            Option.some_if (vendored || included) (name, pkg))
        >>| List.filter_opt >>| Package.Name.Map.of_list_exn >>| Option.some)

let get_mask () = Memo.Lazy.force conf

let packages_of_project project =
  let+ t = Memo.Lazy.force conf in
  let packages = Dune_project.packages project in
  match t with
  | None -> packages
  | Some mask ->
    Package.Name.Map.merge packages mask ~f:(fun _ p mask ->
        match (p, mask) with
        | Some _, Some _ -> p
        | _ -> None)

let filter_out_stanzas_from_hidden_packages ~visible_pkgs =
  List.filter_map ~f:(fun stanza ->
      let include_stanza =
        match Dune_file.stanza_package stanza with
        | None -> true
        | Some package ->
          let name = Package.name package in
          Package.Name.Map.mem visible_pkgs name
      in
      if include_stanza then Some stanza
      else
        match stanza with
        | Dune_file.Library l ->
          let open Option.O in
          let+ redirect = Dune_file.Library_redirect.Local.of_private_lib l in
          Dune_file.Library_redirect redirect
        | _ -> None)

let filtered_stanzas_by_contexts =
  Memo.lazy_ @@ fun () ->
  let* contexts = Context.DB.all () in
  let+ { Dune_load.dune_files; packages = _; projects = _ } =
    Dune_load.load ()
  in
  Context_name.Map.of_list_map_exn contexts ~f:(fun context ->
      ( context.name
      , Memo.lazy_ @@ fun () ->
        let* stanzas = Dune_load.Dune_files.eval ~context dune_files in
        let+ only_packages = Memo.Lazy.force conf in
        match only_packages with
        | None -> stanzas
        | Some visible_pkgs ->
          List.map stanzas ~f:(fun (dir_conf : Dune_file.t) ->
              { dir_conf with
                stanzas =
                  filter_out_stanzas_from_hidden_packages ~visible_pkgs
                    dir_conf.stanzas
              }) ))

let filtered_stanzas (context : Context.t) =
  let* map = Memo.Lazy.force filtered_stanzas_by_contexts in
  Context_name.Map.find_exn map context.name |> Memo.Lazy.force

let get () =
  let* { Dune_load.dune_files = _; packages; projects = _ } =
    Dune_load.load ()
  in
  let+ only_packages = Memo.Lazy.force conf in
  Option.value only_packages ~default:packages

let stanzas_in_dir dir =
  if Path.Build.is_root dir then Memo.return None
  else
    let* dune_file = Dune_load.Dune_files.in_dir dir in
    match dune_file with
    | None -> Memo.return None
    | Some dune_file ->
      let* only_packages = Memo.Lazy.force conf in
      let stanzas =
        match only_packages with
        | None -> dune_file.stanzas
        | Some visible_pkgs ->
          filter_out_stanzas_from_hidden_packages ~visible_pkgs
            dune_file.stanzas
      in
      Memo.return (Some { dune_file with stanzas })
