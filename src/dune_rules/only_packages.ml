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

type t = Package.t Package.Name.Map.t option

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

let get () = Memo.Lazy.force conf

let packages_of_project project =
  let+ t = get () in
  let packages = Dune_project.packages project in
  match t with
  | None -> packages
  | Some mask ->
    Package.Name.Map.merge packages mask ~f:(fun _ p mask ->
        match (p, mask) with
        | Some _, Some _ -> p
        | _ -> None)
