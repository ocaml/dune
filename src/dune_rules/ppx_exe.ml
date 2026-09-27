open Import
open Memo.O

(* Encoded representation of a set of library names + scope *)
module Key : sig
  type encoded = Digest.t

  module Decoded : sig
    type scope = private
      { project : Loaded_project.Identity.t
      ; dir : Path.Local.t
      }

    type t = private
      { pps : Lib_name.t list
      ; scope : scope option
      ; opaque_packages : Package.Name.t Lib_name.Map.t
      }

    val of_libs : context:Context_name.t -> Lib.t list -> scope:Scope.t -> t Memo.t
  end

  val encode : Decoded.t -> encoded
  val decode : encoded -> Decoded.t
end = struct
  type encoded = Digest.t

  module Decoded = struct
    type scope =
      { project : Loaded_project.Identity.t
      ; dir : Path.Local.t
      }

    type t =
      { pps : Lib_name.t list
      ; scope : scope option
      ; opaque_packages : Package.Name.t Lib_name.Map.t
      }

    let equal_scope x y =
      Loaded_project.Identity.equal x.project y.project && Path.Local.equal x.dir y.dir
    ;;

    let scope_repr =
      Repr.record
        "ppx-scope"
        [ Repr.field "project" Loaded_project.Identity.repr ~get:(fun t -> t.project)
        ; Repr.field "dir" Path.Local.repr ~get:(fun t -> t.dir)
        ]
    ;;

    let equal x y =
      List.equal Lib_name.equal x.pps y.pps
      && Option.equal equal_scope x.scope y.scope
      && Lib_name.Map.equal ~equal:Package.Name.equal x.opaque_packages y.opaque_packages
    ;;

    let to_string { pps; scope; opaque_packages } =
      let s =
        List.map pps ~f:(fun name ->
          match Lib_name.Map.find opaque_packages name with
          | None -> Lib_name.to_string name
          | Some package ->
            sprintf
              "%s (package %s)"
              (Lib_name.to_string name)
              (Package.Name.to_string package))
        |> String.enumerate_and
      in
      match scope with
      | None -> s
      | Some { project; dir } ->
        sprintf
          "%s (in scope: %s at %s)"
          s
          (Loaded_project.Identity.to_dyn project |> Dyn.to_string)
          (Path.Local.to_string_maybe_quoted dir)
    ;;

    let of_libs ~context libs ~scope:consumer_scope =
      let* opaque_packages =
        Memo.List.filter_map libs ~f:(fun lib ->
          match Lib_info.package (Lib.info lib) with
          | None -> Memo.return None
          | Some name ->
            let+ package = Opam_package.find context name in
            (match package with
             | Some package
               when Path.is_descendant
                      (Lib_info.src_dir (Lib.info lib))
                      ~of_:(Path.build package.paths.target_dir) ->
               Some (Lib.name lib, name)
             | None | Some _ -> None))
        >>| Lib_name.Map.of_list_reduce ~f:(fun package _ -> package)
      in
      let pps =
        (let compare a b = Lib_name.compare (Lib.name a) (Lib.name b) in
         List.sort libs ~compare)
        |> List.map ~f:Lib.name
      in
      let* private_scope =
        Memo.List.fold_left libs ~init:None ~f:(fun acc lib ->
          let info = Lib.info lib in
          match Lib_info.status info with
          | Installed_private _ | Installed _ | Public _ -> Memo.return acc
          | Private (project, _) ->
            let output_dir = Path.as_in_build_dir_exn (Lib_info.src_dir info) in
            let+ loaded_project = Dune_load.find_loaded_project ~dir:output_dir in
            if not (Dune_project.equal project (Loaded_project.project loaded_project))
            then
              Code_error.raise
                "Private PPX library project does not match its loaded project"
                [ "library_project", Dune_project.to_dyn project
                ; "loaded_project", Loaded_project.to_dyn loaded_project
                ];
            let source_dir =
              Loaded_project.source_path loaded_project output_dir |> Option.value_exn
            in
            let dir =
              Source_path.descendant
                source_dir
                ~of_:(Loaded_project.source_root loaded_project)
              |> Option.value_exn
            in
            let scope = { project = Loaded_project.identity loaded_project; dir } in
            Option.merge acc (Some scope) ~f:(fun a b ->
              if not (Loaded_project.Identity.equal a.project b.project)
              then
                Code_error.raise
                  "Private PPX libraries belong to different loaded projects"
                  [ "first", Loaded_project.Identity.to_dyn a.project
                  ; "second", Loaded_project.Identity.to_dyn b.project
                  ];
              { a with dir = Ordering.min Path.Local.compare a.dir b.dir }))
      and* consumer_loaded_project =
        Dune_load.find_loaded_project ~dir:(Scope.root consumer_scope)
      in
      let consumer_scope =
        match
          Build_partition.purpose (Loaded_project.partition consumer_loaded_project)
        with
        | Workspace -> None
        | Mounted ->
          Some
            { project = Loaded_project.identity consumer_loaded_project
            ; dir = Path.Local.root
            }
      in
      let scope =
        match consumer_scope, private_scope with
        | None, scope | scope, None -> scope
        | Some consumer, Some private_ ->
          if not (Loaded_project.Identity.equal consumer.project private_.project)
          then
            Code_error.raise
              "Private PPX library is outside the consuming mounted project"
              [ "consumer", Loaded_project.Identity.to_dyn consumer.project
              ; "private", Loaded_project.Identity.to_dyn private_.project
              ];
          Some private_
      in
      Memo.return { pps; scope; opaque_packages }
    ;;
  end

  let reverse_table : (Digest.t, Decoded.t) Table.t = Table.create (module Digest) 128

  let encode ({ Decoded.pps; scope; opaque_packages } as x) =
    let y =
      if Lib_name.Map.is_empty opaque_packages
      then
        Digest.repr
          Repr.(pair (list Lib_name.repr) (option Decoded.scope_repr))
          (pps, scope)
      else
        Digest.repr
          Repr.(
            triple
              (list Lib_name.repr)
              (option Decoded.scope_repr)
              (list (pair Lib_name.repr Package.Name.repr)))
          (pps, scope, Lib_name.Map.to_list opaque_packages)
    in
    match Table.find reverse_table y with
    | None ->
      Table.set reverse_table y x;
      y
    | Some x' ->
      if Decoded.equal x x'
      then y
      else
        User_error.raise
          [ Pp.textf "Hash collision between set of ppx drivers:"
          ; Pp.textf "- cache : %s" (Decoded.to_string x')
          ; Pp.textf "- fetch : %s" (Decoded.to_string x)
          ]
  ;;

  let decode y =
    match Table.find reverse_table y with
    | Some x -> x
    | None ->
      User_error.raise
        [ Pp.textf
            "I don't know what ppx rewriters set %s correspond to."
            (Digest.to_string y)
        ]
  ;;
end

let ppx_exe_path root ~key = Path.Build.relative root (".ppx/" ^ key ^ "/ppx.exe")

let ppx_driver_exe (ctx : Context.t) ~scope libs =
  let* host = Context.host ctx in
  let* decoded = Key.Decoded.of_libs ~context:(Context.name host) libs ~scope in
  let key = Digest.to_string (Key.encode decoded) in
  let+ root =
    match decoded.scope with
    | None -> Memo.return (Context.build_dir host)
    | Some { project; _ } ->
      let+ loaded_project =
        Dune_load.find_loaded_project_by_identity
          ~context:(Context.name host)
          ~identity:project
      in
      (match Build_partition.purpose (Loaded_project.partition loaded_project) with
       | Workspace -> Context.build_dir host
       | Mounted -> Loaded_project.output_root loaded_project)
  in
  ppx_exe_path root ~key
;;

let get_ppx_exe ctx ~scope pps =
  let open Resolve.Memo.O in
  let* libs = Lib.DB.resolve_pps (Scope.libs scope) pps in
  ppx_driver_exe ctx ~scope libs |> Resolve.Memo.lift_memo
;;
