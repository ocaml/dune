open Import

let default_context_flags (ctx : Build_context.t) ocaml_config ~project =
  let cflags = Ocaml_config.ocamlc_cflags ocaml_config in
  let c, cxx =
    let cxxflags =
      List.filter cflags ~f:(fun s -> not (String.is_prefix s ~prefix:"-std="))
    in
    match Dune_project.use_standard_c_and_cxx_flags project with
    | None | Some false -> Action_builder.(return cflags, return cxxflags)
    | Some true ->
      let fdiagnostics_color =
        Cxx_flags.ccomp_type ctx |> Action_builder.map ~f:Cxx_flags.fdiagnostics_color
      in
      let open Action_builder.O in
      let c =
        let+ fdiagnostics_color = fdiagnostics_color in
        List.concat
          [ cflags; Ocaml_config.ocamlc_cppflags ocaml_config; fdiagnostics_color ]
      in
      let cxx =
        let+ fdiagnostics_color = fdiagnostics_color
        and+ db_flags = Cxx_flags.get_flags ~for_:Compile ctx in
        List.concat [ db_flags; cxxflags; fdiagnostics_color ]
      in
      c, cxx
  in
  Foreign_language.Dict.make ~c ~cxx
;;

module Env_tree : sig
  type t

  val force_artifacts : t -> unit Memo.t
  val context : t -> Context.t
  val get_node : t -> dir:Path.Build.t -> Env_node.t Memo.t
  val get_context_env : t -> Env.t

  val create
    :  context:Context.t
    -> host_env_tree:t option
    -> default_env:Env_node.t Memo.Lazy.t
    -> root_expander:Expander.t
    -> artifacts:Artifacts.t
    -> context_env:Env.t
    -> t

  val artifacts_host : t -> dir:Path.Build.t -> Artifacts.t Memo.t
  val expander : t -> dir:Path.Build.t -> Expander.t Memo.t
end = struct
  open Memo.O

  type t =
    { context : Context.t
    ; context_env : Env.t (** context env with additional variables *)
    ; default_env : Env_node.t Memo.Lazy.t
    ; host : t option
    ; root_expander : Expander.t
    ; artifacts : Artifacts.t
    ; get_node : Path.Build.t -> Env_node.t Memo.t
    }

  let force_artifacts { artifacts; _ } = Artifacts.force artifacts
  let context t = t.context
  let get_node t ~dir = t.get_node dir
  let get_context_env t = t.context_env

  let artifacts_host t ~dir =
    let artifacts t ~dir = get_node t ~dir >>= Env_node.artifacts in
    match t.host with
    | None -> artifacts t ~dir
    | Some host ->
      let dir =
        Path.Build.drop_build_context_exn dir
        |> Path.Build.append_source (Context.build_dir host.context)
      in
      artifacts host ~dir
  ;;

  let external_env t ~dir = get_node t ~dir >>= Env_node.external_env

  let scope_host ~scope (context : Context.t) =
    match Context.for_host context with
    | None -> Memo.return scope
    | Some host ->
      let* dir =
        let root = Scope.root scope in
        let src = Path.Build.drop_build_context_exn root in
        let+ host = host in
        Path.Build.append_source (Context.build_dir host) src
      in
      Scope.DB.find_by_dir dir
  ;;

  let expander_for_artifacts ~scope ~external_env ~root_expander ~dir =
    let+ scope_host = scope_host ~scope (Expander.context root_expander) in
    Expander.extend_env root_expander ~env:external_env
    |> Expander.set_scope ~scope ~scope_host
    |> Expander.set_dir ~dir
  ;;

  let extend_expander t ~dir ~expander_for_artifacts =
    let+ artifacts_host = artifacts_host t ~dir
    and+ bindings =
      let+ inline_tests = get_node t ~dir >>= Env_node.inline_tests in
      let str = Dune_env.Stanza.Inline_tests.to_string inline_tests in
      Pform.Map.singleton (Var Inline_tests) [ Value.String str ]
    in
    Expander.add_bindings ~bindings expander_for_artifacts
    |> Expander.set_artifacts ~artifacts_host
  ;;

  let expander t ~dir =
    let* node = get_node t ~dir
    and+ external_env = external_env t ~dir in
    let* expander_for_artifacts =
      let scope = Env_node.scope node in
      expander_for_artifacts ~scope ~external_env ~root_expander:t.root_expander ~dir
    in
    let+ expander = extend_expander t ~dir ~expander_for_artifacts in
    Expander.set_foreign_flags expander ~f:(fun ~dir ->
      get_node t ~dir >>| Env_node.foreign_flags)
  ;;

  let get_env_stanza ~dir =
    let open Memo.O in
    let+ stanzas = Only_packages.stanzas_in_dir dir in
    Option.value ~default:Dune_env.Stanza.empty
    @@
    let open Option.O in
    let* stanzas = stanzas in
    List.find_map stanzas.stanzas ~f:(function
      | Dune_env.T config -> Some config
      | _ -> None)
  ;;

  let get_impl t dir =
    let* scope = Scope.DB.find_by_dir dir in
    let project = Scope.project scope in
    let inherit_from =
      if Path.Build.equal dir (Scope.root scope)
      then (
        let format_config = Dune_project.format_config project in
        Memo.lazy_ (fun () ->
          let+ default_env = Memo.Lazy.force t.default_env in
          Env_node.set_format_config default_env format_config))
      else (
        match Path.Build.parent dir with
        | None ->
          Code_error.raise
            "Super_context.Env.get called on invalid directory"
            [ "dir", Path.Build.to_dyn dir ]
        | Some parent -> Memo.lazy_ (fun () -> get_node t ~dir:parent))
    in
    let* config_stanza = get_env_stanza ~dir in
    let build_context = Context.build_context t.context in
    let+ default_context_flags =
      let+ ocaml = Context.ocaml t.context in
      default_context_flags build_context ocaml.ocaml_config ~project
    in
    let expander_for_artifacts =
      Memo.lazy_ (fun () ->
        let* external_env = external_env t ~dir in
        expander_for_artifacts ~scope ~root_expander:t.root_expander ~external_env ~dir)
    in
    let expander =
      Memo.lazy_ (fun () ->
        let* expander_for_artifacts = Memo.Lazy.force expander_for_artifacts in
        extend_expander t ~dir ~expander_for_artifacts >>| Expander.set_dir ~dir)
    in
    let profile = Context.profile t.context in
    Env_node.make
      build_context
      ~dir
      ~scope
      ~config_stanza
      ~inherit_from:(Some inherit_from)
      ~profile
      ~expander
      ~expander_for_artifacts
      ~default_context_flags
      ~default_env:t.context_env
      ~default_artifacts:t.artifacts
      ~default_bin_annot:true
  ;;

  (* Here we jump through some hoops to construct [t] as well as create a
     memoization table that has access to [t] and is used in [t.get_node].

     Morally, the code below is just:

     let rec env_tree = ... and memo = ... in env_tree

     However, the right-hand side of [memo] is not allowed in a recursive let
     binding. To work around this limitation, we place the functions into a
     recursive module [Rec]. Since recursive let-modules are not allowed either,
     we need to also wrap [Rec] inside a non-recursive module [Non_rec]. *)
  let create ~context ~host_env_tree ~default_env ~root_expander ~artifacts ~context_env =
    let module Non_rec = struct
      module rec Rec : sig
        val env_tree : unit -> t
        val memo : Path.Build.t -> Env_node.t Memo.t
      end = struct
        let env_tree =
          { context
          ; context_env
          ; default_env
          ; host = host_env_tree
          ; root_expander
          ; artifacts
          ; get_node = Rec.memo
          }
        ;;

        let memo =
          Memo.exec
            (Memo.create
               "env-nodes-memo"
               ~input:(module Path.Build)
               (fun path -> get_impl env_tree path))
        ;;

        let env_tree () = env_tree
      end
    end
    in
    Non_rec.Rec.env_tree ()
  ;;
end

type t = Env_tree.t

let context t = Env_tree.context t
let context_env t = Env_tree.get_context_env t
let equal : t -> t -> bool = phys_equal
let hash t = Context.hash (Env_tree.context t)
let to_dyn_concise t = Context.to_dyn_concise (Env_tree.context t)
let to_dyn t = Context.to_dyn (Env_tree.context t)
let expander t ~dir = Env_tree.expander t ~dir

open Memo.O

let extend_action t ~dir action =
  let open Action_builder.O in
  let+ (action : Action.Full.t) = action
  and+ env =
    Action_builder.of_memo
      (let open Memo.O in
       Env_tree.get_node t ~dir >>= Env_node.external_env)
  in
  Action.Full.add_env env action
  |> Action.Full.map ~f:(function
    | Chdir _ as a -> a
    | a -> Chdir (Path.build (Context.build_dir (Env_tree.context t)), a))
;;

let make_rule t ?mode ?loc ~dir { Action_builder.With_targets.build; targets } =
  let build = extend_action t build ~dir in
  Rule.make ?mode ~info:(Rule.Info.of_loc_opt loc) ~targets build
;;

let add_rule t ?mode ?loc ~dir build =
  let rule = make_rule t ?mode ?loc ~dir build in
  Rules.Produce.rule rule
;;

let add_rule_get_targets t ?mode ?loc ~dir build =
  let rule = make_rule t ?mode ?loc ~dir build in
  let+ () = Rules.Produce.rule rule in
  rule.targets
;;

let add_rules t ?loc ~dir builds = Memo.parallel_iter builds ~f:(add_rule ?loc t ~dir)

let add_alias_action t alias ~dir ~loc action =
  let build = extend_action t action ~dir in
  Rules.Produce.Alias.add_action alias ~loc build
;;

let env_node = Env_tree.get_node
let bin_annot t ~dir = Env_tree.get_node t ~dir >>= Env_node.bin_annot

let resolve_program_memo t ~dir ?hint ~loc bin =
  let* artifacts = Env_tree.artifacts_host t ~dir in
  Artifacts.binary ?hint ~loc artifacts bin
;;

let resolve_program t ~dir ?hint ~loc bin =
  Action_builder.of_memo @@ resolve_program_memo t ~dir ?hint ~loc bin
;;

let add_packages_env context ~base stanzas packages =
  let+ env_dune_dir_locations =
    let init =
      match Stdune.Env.get base Dune_site_private.dune_dir_locations_env_var with
      | None -> []
      | Some var ->
        (match Dune_site_private.decode_dune_dir_locations var with
         | Some s -> s
         | None ->
           User_error.raise
             [ Pp.textf
                 "Invalid env var %s=%S"
                 Dune_site_private.dune_dir_locations_env_var
                 var
             ])
    in
    let+ package_sections =
      (* Add the section of the site mentioned in stanzas (it could be a site
         of an external package) *)
      let add_in_package_section m pkg section =
        Package.Name.Map.update m pkg ~f:(function
          | None -> Some (Section.Set.singleton section)
          | Some s -> Some (Section.Set.add s section))
      in
      let+ package_sections =
        let* package_db = Package_db.create context in
        Dune_file.Memo_fold.fold_stanzas
          stanzas
          ~init:Package.Name.Map.empty
          ~f:(fun _ stanza acc ->
            let add_in_package_sites pkg_name site loc =
              let+ pkg = Package_db.find_package package_db pkg_name in
              match pkg with
              | None ->
                (* Really ugly to supress errors like this. Instead,
                   executables that rely on sites should declare that
                   in their dependencies *)
                acc
              | Some pkg ->
                let section =
                  Package_db.section_of_any_package_site pkg pkg_name loc site
                in
                add_in_package_section acc pkg_name section
            in
            match stanza with
            | Dune_file.Install { section = Site { pkg; site; loc }; _ } ->
              add_in_package_sites pkg site loc
            | Dune_file.Plugin { site = loc, (pkg, site); _ } ->
              add_in_package_sites pkg site loc
            | _ -> Memo.return acc)
      in
      (* Add the site of the local package: it should only useful for making
         sure that at least one location is given to the site of local package
         because if the site is used it should already be in
         [packages_sections] *)
      Package.Name.Map.foldi
        packages
        ~init:package_sections
        ~f:(fun package_name (package : Package.t) acc ->
          Site.Map.fold package.sites ~init:acc ~f:(fun section acc ->
            add_in_package_section acc package_name section))
    in
    let roots =
      Install.Context.dir ~context |> Path.build |> Install.Roots.opam_from_prefix
    in
    Package.Name.Map.foldi ~init package_sections ~f:(fun package_name sections init ->
      let paths = Install.Paths.make ~package:package_name ~roots in
      Section.Set.fold sections ~init ~f:(fun section acc ->
        let package = Package.Name.to_string package_name in
        let dir = Path.to_absolute_filename (Install.Paths.get paths section) in
        { Dune_site_private.package; dir; section } :: acc))
  in
  if List.is_empty env_dune_dir_locations
  then base
  else
    Stdune.Env.add
      base
      ~var:Dune_site_private.dune_dir_locations_env_var
      ~value:(Dune_site_private.encode_dune_dir_locations env_dune_dir_locations)
;;

let make_default_env_node
  ocaml_config
  (context : Build_context.t)
  profile
  root_expander
  (env_nodes : Context.Env_nodes.t)
  ~root_env
  ~(artifacts : Artifacts.t)
  =
  let make ~inherit_from ~config_stanza =
    let config_stanza = Option.value config_stanza ~default:Dune_env.Stanza.empty in
    let dir = context.build_dir in
    let+ scope = Scope.DB.find_by_dir dir in
    let project = Scope.project scope in
    let default_context_flags = default_context_flags context ocaml_config ~project in
    let expander_for_artifacts =
      Memo.lazy_ (fun () ->
        Code_error.raise "[expander_for_artifacts] in [default_env] is undefined" [])
    in
    Dune_env.Stanza.fire_hooks config_stanza ~profile;
    let expander = Memo.Lazy.of_val (Expander.set_dir ~dir root_expander) in
    Env_node.make
      context
      ~dir
      ~scope
      ~inherit_from
      ~config_stanza
      ~profile
      ~expander
      ~expander_for_artifacts
      ~default_context_flags
      ~default_env:root_env
      ~default_artifacts:artifacts
      ~default_bin_annot:true
  in
  make
    ~config_stanza:env_nodes.context
    ~inherit_from:
      (Some
         (Memo.lazy_ (fun () ->
            make ~inherit_from:None ~config_stanza:env_nodes.workspace)))
;;

let make_root_env (context : Context.t) ~(host : t option) =
  let name = Context.name context in
  let roots =
    Install.Roots.make ~relative:Path.Build.relative (Install.Context.dir ~context:name)
  in
  Context.installed_env context
  |> Install.Roots.add_to_env roots
  |> Env.update ~var:Env_path.var ~f:(fun _PATH ->
    let context, _PATH =
      match host with
      | None -> context, _PATH
      | Some host ->
        let context = Env_tree.context host in
        let _PATH = Env.get (Context.installed_env context) Env_path.var in
        context, _PATH
    in
    let name = Context.name context in
    Some (Bin.cons_path (Path.build (Install.Context.bin_dir ~context:name)) ~_PATH))
;;

let dune_sites_env ~default_ocamlpath ~stdlib =
  [ Dune_site_private.dune_ocaml_stdlib_env_var, Path.to_absolute_filename stdlib
  ; ( Dune_site_private.dune_ocaml_hardcoded_env_var
    , List.map ~f:Path.to_absolute_filename default_ocamlpath
      |> String.concat ~sep:(Char.escaped Findlib_config.ocamlpath_sep) )
  ; ( Dune_site_private.dune_sourceroot_env_var
    , Path.to_absolute_filename (Path.source Path.Source.root) )
  ]
  |> String.Map.of_list_exn
  |> Env.of_string_map
;;

let create ~(context : Context.t) ~(host : t option) ~packages ~stanzas =
  let* ocaml = Context.ocaml context in
  let* expander_env =
    let+ default_ocamlpath = Context.default_ocamlpath context in
    Env.extend_env
      (make_root_env context ~host)
      (dune_sites_env ~default_ocamlpath ~stdlib:ocaml.lib_config.stdlib_dir)
  in
  let public_libs = Scope.DB.public_libs context in
  let artifacts = Artifacts_db.get context in
  let+ root_expander =
    let artifacts_host, public_libs_host, context_host =
      match Context.for_host context with
      | None -> artifacts, public_libs, Memo.return context
      | Some host ->
        let artifacts = host >>= Artifacts_db.get in
        let public_libs = host >>= Scope.DB.public_libs in
        artifacts, public_libs, host
    in
    let+ scope = Scope.DB.find_by_dir (Context.build_dir context)
    and+ public_libs = public_libs
    and+ artifacts_host = artifacts_host
    and+ public_libs_host = public_libs_host
    and+ scope_host = context_host >>| Context.build_dir >>= Scope.DB.find_by_dir in
    Expander.make_root
      ~scope
      ~scope_host
      ~context
      ~env:expander_env
      ~lib_artifacts:public_libs
      ~artifacts_host
      ~lib_artifacts_host:public_libs_host
  and+ artifacts = artifacts
  and+ root_env =
    add_packages_env (Context.name context) ~base:expander_env stanzas packages
  in
  (* Env node that represents the environment configured for the workspace. It
     is used as default at the root of every project in the workspace. *)
  let default_env =
    let profile = Context.profile context in
    Memo.lazy_ ~name:"default_env" (fun () ->
      make_default_env_node
        ocaml.ocaml_config
        (Context.build_context context)
        profile
        root_expander
        (Context.env_nodes context)
        ~root_env
        ~artifacts)
  in
  Env_tree.create
    ~context
    ~default_env
    ~host_env_tree:host
    ~root_expander
    ~artifacts
    ~context_env:root_env
;;

let all =
  Memo.lazy_ ~name:"Super_context.all" (fun () ->
    let open Memo.O in
    let* packages = Only_packages.get ()
    and* contexts = Context.DB.all () in
    let rec sctxs =
      lazy
        (Context_name.Map.of_list_map_exn contexts ~f:(fun (c : Context.t) ->
           Context.name c, Memo.Lazy.create ~name:"make_sctx" (fun () -> make_sctx c)))
    and make_sctx (context : Context.t) =
      let host () =
        match Context.for_host context with
        | None -> Memo.return None
        | Some h ->
          let+ sctx =
            let* h = h in
            Memo.Lazy.force
              (Context_name.Map.find_exn (Lazy.force sctxs) (Context.name h))
          in
          Some sctx
      in
      let* host, stanzas =
        Memo.fork_and_join host (fun () ->
          Only_packages.filtered_stanzas (Context.name context))
      in
      create ~host ~context ~packages ~stanzas
    in
    Lazy.force sctxs
    |> Context_name.Map.to_list
    |> Memo.parallel_map ~f:(fun (name, sctx) ->
      let+ sctx = Memo.Lazy.force sctx in
      name, sctx)
    >>| Context_name.Map.of_list_exn)
;;

let find name =
  let open Memo.O in
  let+ all = Memo.Lazy.force all in
  Context_name.Map.find all name
;;

let find_exn name =
  let open Memo.O in
  let+ all = Memo.Lazy.force all in
  Context_name.Map.find_exn all name
;;

let all_init_deferred () =
  let* all = Memo.Lazy.force all in
  Context_name.Map.values all |> Memo.parallel_iter ~f:Env_tree.force_artifacts
;;

module As_memo_key = struct
  type nonrec t = t

  let equal = equal
  let hash = hash
  let to_dyn = to_dyn_concise

  module And_package = struct
    type nonrec t = t * Package.t

    let hash = Tuple.T2.hash hash Package.hash
    let equal (x1, y1) (x2, y2) = equal x1 x2 && Package.equal y1 y2
    let to_dyn (s, p) = Dyn.Tuple [ to_dyn s; Package.to_dyn p ]
  end
end
