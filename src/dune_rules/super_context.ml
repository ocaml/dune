open Import

let default_context_flags (ctx : Context.t) ~project =
  let cflags = Ocaml_config.ocamlc_cflags ctx.ocaml_config in
  let cxxflags =
    List.filter cflags ~f:(fun s -> not (String.is_prefix s ~prefix:"-std="))
  in
  let c, cxx =
    match Dune_project.use_standard_c_and_cxx_flags project with
    | None | Some false ->
      (Action_builder.return cflags, Action_builder.return cxxflags)
    | Some true ->
      let c = cflags @ Ocaml_config.ocamlc_cppflags ctx.ocaml_config in
      let cxx =
        let open Action_builder.O in
        let+ db_flags = Cxx_flags.get_flags ~for_:Compile ctx in
        db_flags @ cxxflags
      in
      (Action_builder.return c, cxx)
  in
  Foreign_language.Dict.make ~c ~cxx

module Env_tree : sig
  type t

  val context : t -> Context.t

  val get_node : t -> dir:Path.Build.t -> Env_node.t Memo.t

  val get_context_env : t -> Env.t

  val create :
       context:Context.t
    -> host_env_tree:t option
    -> default_env:Env_node.t Memo.Lazy.t
    -> root_expander:Expander.t
    -> bin_artifacts:Artifacts.Bin.t
    -> context_env:Env.t
    -> t

  val bin_artifacts_host : t -> dir:Path.Build.t -> Artifacts.Bin.t Memo.t

  val expander : t -> dir:Path.Build.t -> Expander.t Memo.t
end = struct
  open Memo.O

  type t =
    { context : Context.t
    ; context_env : Env.t  (** context env with additional variables *)
    ; default_env : Env_node.t Memo.Lazy.t
    ; host : t option
    ; root_expander : Expander.t
    ; bin_artifacts : Artifacts.Bin.t
    ; get_node : Path.Build.t -> Env_node.t Memo.t
    }

  let context t = t.context

  let get_node t ~dir = t.get_node dir

  let get_context_env t = t.context_env

  let bin_artifacts_host t ~dir =
    let bin_artifacts t ~dir = get_node t ~dir >>= Env_node.bin_artifacts in
    match t.host with
    | None -> bin_artifacts t ~dir
    | Some host ->
      let dir =
        Path.Build.drop_build_context_exn dir
        |> Path.Build.append_source host.context.build_dir
      in
      bin_artifacts host ~dir

  let external_env t ~dir = get_node t ~dir >>= Env_node.external_env

  let expander_for_artifacts ~scope ~external_env ~root_expander ~dir =
    Expander.extend_env root_expander ~env:external_env
    |> Expander.set_scope ~scope |> Expander.set_dir ~dir

  let extend_expander t ~dir ~expander_for_artifacts =
    let* bin_artifacts_host = bin_artifacts_host t ~dir in
    let+ bindings =
      let+ inline_tests = get_node t ~dir >>= Env_node.inline_tests in
      let str = Dune_env.Stanza.Inline_tests.to_string inline_tests in
      Pform.Map.singleton (Var Inline_tests) [ Value.String str ]
    in
    expander_for_artifacts
    |> Expander.add_bindings ~bindings
    |> Expander.set_bin_artifacts ~bin_artifacts_host

  let expander t ~dir =
    let* node = get_node t ~dir in
    let* external_env = external_env t ~dir in
    let scope = Env_node.scope node in
    let expander_for_artifacts =
      expander_for_artifacts ~scope ~external_env ~root_expander:t.root_expander
        ~dir
    in
    let+ expander = extend_expander t ~dir ~expander_for_artifacts in
    Expander.set_foreign_flags expander ~f:(fun ~dir ->
        get_node t ~dir >>| Env_node.foreign_flags)

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

  let get_impl t dir =
    (* We recompute the scope on every recursive call, even though it should be
       unchanged. If this becomes a problem, we can memoize [find_by_dir]. *)
    let* scope = Scope.DB.find_by_dir dir in
    let inherit_from =
      if Path.Build.equal dir (Scope.root scope) then
        let format_config = Dune_project.format_config (Scope.project scope) in
        Memo.lazy_ (fun () ->
            let+ default_env = Memo.Lazy.force t.default_env in
            Env_node.set_format_config default_env format_config)
      else
        match Path.Build.parent dir with
        | None ->
          Code_error.raise "Super_context.Env.get called on invalid directory"
            [ ("dir", Path.Build.to_dyn dir) ]
        | Some parent -> Memo.lazy_ (fun () -> get_node t ~dir:parent)
    in
    let+ config_stanza = get_env_stanza ~dir in
    let project = Scope.project scope in
    let default_context_flags = default_context_flags t.context ~project in
    let expander_for_artifacts =
      Memo.lazy_ (fun () ->
          let+ external_env = external_env t ~dir in
          expander_for_artifacts ~scope ~root_expander:t.root_expander
            ~external_env ~dir)
    in
    let expander =
      Memo.lazy_ (fun () ->
          let* expander_for_artifacts =
            Memo.Lazy.force expander_for_artifacts
          in
          extend_expander t ~dir ~expander_for_artifacts)
    in
    let default_cxx_link_flags = Cxx_flags.get_flags ~for_:Link t.context in
    Env_node.make ~dir ~scope ~config_stanza ~inherit_from:(Some inherit_from)
      ~profile:t.context.profile ~expander ~expander_for_artifacts
      ~default_context_flags ~default_env:t.context_env
      ~default_bin_artifacts:t.bin_artifacts ~default_cxx_link_flags

  (* Here we jump through some hoops to construct [t] as well as create a
     memoization table that has access to [t] and is used in [t.get_node].

     Morally, the code below is just:

     let rec env_tree = ... and memo = ... in env_tree

     However, the right-hand side of [memo] is not allowed in a recursive let
     binding. To work around this limitation, we place the functions into a
     recursive module [Rec]. Since recursive let-modules are not allowed either,
     we need to also wrap [Rec] inside a non-recursive module [Non_rec]. *)
  let create ~context ~host_env_tree ~default_env ~root_expander ~bin_artifacts
      ~context_env =
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
          ; bin_artifacts
          ; get_node = Rec.memo
          }

        let memo =
          Memo.exec
            (Memo.create "env-nodes-memo"
               ~input:(module Path.Build)
               (fun path -> get_impl env_tree path))

        let env_tree () = env_tree
      end
    end in
    Non_rec.Rec.env_tree ()
end

type t = Env_tree.t

let context t = Env_tree.context t

let context_env t = Env_tree.get_context_env t

let equal = (( == ) : t -> t -> bool)

let hash t = Context.hash (Env_tree.context t)

let to_dyn_concise t = Context.to_dyn_concise (Env_tree.context t)

let to_dyn t = Context.to_dyn (Env_tree.context t)

let expander t ~dir = Env_tree.expander t ~dir

open Memo.O

let extend_action t ~dir build =
  let open Action_builder.O in
  let+ (act : Action.Full.t) = build
  and+ env =
    Action_builder.of_memo
      (let open Memo.O in
      Env_tree.get_node t ~dir >>= Env_node.external_env)
  in
  act |> Action.Full.add_env env
  |> Action.Full.map ~f:(function
       | Chdir _ as a -> a
       | a -> Chdir (Path.build (Env_tree.context t).build_dir, a))

let make_rule t ?mode ?loc ~dir { Action_builder.With_targets.build; targets } =
  let build = extend_action t build ~dir in
  Rule.make ?mode ~info:(Rule.Info.of_loc_opt loc)
    ~context:(Some (Context.build_context (Env_tree.context t)))
    ~targets build

let add_rule t ?mode ?loc ~dir build =
  let rule = make_rule t ?mode ?loc ~dir build in
  Rules.Produce.rule rule

let add_rule_get_targets t ?mode ?loc ~dir build =
  let rule = make_rule t ?mode ?loc ~dir build in
  let+ () = Rules.Produce.rule rule in
  rule.targets

let add_rules t ?loc ~dir builds =
  Memo.parallel_iter builds ~f:(add_rule ?loc t ~dir)

let add_alias_action t alias ~dir ~loc action =
  let build = extend_action t action ~dir in
  Rules.Produce.Alias.add_action
    ~context:(Context.build_context (Env_tree.context t))
    alias ~loc build

let build_dir_is_vendored build_dir =
  match Path.Build.drop_build_context build_dir with
  | Some src_dir -> Dune_engine.Source_tree.is_vendored src_dir
  | None -> Memo.return false

let with_vendored_flags ~ocaml_version flags =
  let with_warnings = Ocaml_flags.with_vendored_warnings flags in
  if Ocaml.Version.supports_alerts ocaml_version then
    Ocaml_flags.with_vendored_alerts with_warnings
  else with_warnings

let ocaml_flags t ~dir (spec : Ocaml_flags.Spec.t) =
  let* expander = Env_tree.expander t ~dir in
  let* flags =
    let+ ocaml_flags = Env_tree.get_node t ~dir >>= Env_node.ocaml_flags in
    Ocaml_flags.make ~spec ~default:ocaml_flags
      ~eval:(Expander.expand_and_eval_set expander)
  in
  build_dir_is_vendored dir >>| function
  | true ->
    let ocaml_version = (Env_tree.context t).version in
    with_vendored_flags ~ocaml_version flags
  | false -> flags

let js_of_ocaml_runtest_alias t ~dir =
  let+ js_of_ocaml = Env_tree.get_node t ~dir >>= Env_node.js_of_ocaml in
  match js_of_ocaml.runtest_alias with
  | None -> Alias.Name.runtest
  | Some a -> a

let js_of_ocaml_compilation_mode t ~dir =
  let+ js_of_ocaml = Env_tree.get_node t ~dir >>= Env_node.js_of_ocaml in
  match js_of_ocaml.compilation_mode with
  | None ->
    if Profile.is_dev (Env_tree.context t).profile then
      Js_of_ocaml.Compilation_mode.Separate_compilation
    else Whole_program
  | Some m -> m

let js_of_ocaml_flags t ~dir (spec : Js_of_ocaml.Flags.Spec.t) =
  let+ expander = Env_tree.expander t ~dir
  and+ js_of_ocaml = Env_tree.get_node t ~dir >>= Env_node.js_of_ocaml in
  Js_of_ocaml.Flags.make ~spec ~default:js_of_ocaml.flags
    ~eval:(Expander.expand_and_eval_set expander)

let default_foreign_flags t ~dir ~language =
  Env_tree.get_node t ~dir >>| Env_node.foreign_flags
  >>| (fun dict -> Foreign_language.Dict.get dict language)
  |> Action_builder.of_memo_join

let foreign_flags t ~dir ~expander ~flags ~language =
  let ccg = Context.cc_g (Env_tree.context t) in
  let default = default_foreign_flags t ~dir ~language in
  let open Action_builder.O in
  let name = Foreign_language.proper_name language in
  let flags =
    let+ l = Expander.expand_and_eval_set expander flags ~standard:default in
    l @ ccg
  in
  Action_builder.memoize ~cutoff:(List.equal String.equal)
    (sprintf "%s flags" name) flags

let link_flags t ~dir (spec : Link_flags.Spec.t) =
  let* expander = Env_tree.expander t ~dir in
  let+ link_flags = Env_tree.get_node t ~dir >>= Env_node.link_flags in
  Link_flags.make ~spec ~default:link_flags
    ~eval:(Expander.expand_and_eval_set expander)

let menhir_flags t ~dir ~expander ~flags =
  let t = t in
  let default =
    Env_tree.get_node t ~dir >>| Env_node.menhir_flags
    |> Action_builder.of_memo_join
  in
  Action_builder.memoize ~cutoff:(List.equal String.equal) "menhir flags"
    (Expander.expand_and_eval_set expander flags ~standard:default)

let local_binaries t ~dir = Env_tree.get_node t ~dir >>= Env_node.local_binaries

let env_node = Env_tree.get_node

let odoc t ~dir = Env_tree.get_node t ~dir >>= Env_node.odoc

let coq t ~dir = Env_tree.get_node t ~dir >>= Env_node.coq

let format_config t ~dir = Env_tree.get_node t ~dir >>= Env_node.format_config

let dump_env t ~dir =
  let ocaml_flags = Env_tree.get_node t ~dir >>= Env_node.ocaml_flags in
  let foreign_flags = Env_tree.get_node t ~dir >>| Env_node.foreign_flags in
  let link_flags = Env_tree.get_node t ~dir >>= Env_node.link_flags in
  let menhir_flags = Env_tree.get_node t ~dir >>| Env_node.menhir_flags in
  let coq_flags = Env_tree.get_node t ~dir >>= Env_node.coq in
  let js_of_ocaml = Env_tree.get_node t ~dir >>= Env_node.js_of_ocaml in
  let open Action_builder.O in
  let+ o_dump =
    let* ocaml_flags = Action_builder.of_memo ocaml_flags in
    Ocaml_flags.dump ocaml_flags
  and+ c_dump =
    let* foreign_flags = Action_builder.of_memo foreign_flags in
    let+ c_flags = foreign_flags.c
    and+ cxx_flags = foreign_flags.cxx in
    List.map
      ~f:Dune_lang.Encoder.(pair string (list string))
      [ ("c_flags", c_flags); ("cxx_flags", cxx_flags) ]
  and+ link_flags_dump =
    let* link_flags = Action_builder.of_memo link_flags in
    Link_flags.dump link_flags
  and+ menhir_dump =
    let+ flags = Action_builder.of_memo_join menhir_flags in
    [ ("menhir_flags", flags) ]
    |> List.map ~f:Dune_lang.Encoder.(pair string (list string))
  and+ coq_dump =
    let+ flags = Action_builder.of_memo_join coq_flags in
    [ ("coq_flags", flags) ]
    |> List.map ~f:Dune_lang.Encoder.(pair string (list string))
  and+ jsoo_dump =
    let* jsoo = Action_builder.of_memo js_of_ocaml in
    Js_of_ocaml.Flags.dump jsoo.flags
  in
  List.concat
    [ o_dump; c_dump; link_flags_dump; menhir_dump; coq_dump; jsoo_dump ]

let resolve_program t ~dir ?hint ~loc bin =
  let* bin_artifacts = Env_tree.bin_artifacts_host t ~dir in
  Artifacts.Bin.binary ?hint ~loc bin_artifacts bin

let create ~(context : Context.t) ~host ~packages ~stanzas =
  let* artifacts = Artifacts_db.get context in
  let* root_expander =
    let* artifacts_host, context_host =
      match context.for_host with
      | None -> Memo.return (artifacts, context)
      | Some host ->
        let+ artifacts = Artifacts_db.get host in
        (artifacts, host)
    in
    let* scope = Scope.DB.find_by_dir context.build_dir in
    let* scope_host = Scope.DB.find_by_dir context_host.build_dir in
    Expander.make ~scope ~scope_host ~context
      ~lib_artifacts:artifacts.public_libs
      ~bin_artifacts_host:artifacts_host.bin
      ~lib_artifacts_host:artifacts_host.public_libs
  in
  let+ context_env =
    let+ package_sections =
      (* Add the section of the site mentioned in stanzas (it could be a site
         of an external package) *)
      let add_in_package_section m pkg section =
        Package.Name.Map.update m pkg ~f:(function
          | None -> Some (Section.Set.singleton section)
          | Some s -> Some (Section.Set.add s section))
      in
      let+ package_sections =
        let* sites = Sites.create context in
        Dune_file.Memo_fold.fold_stanzas stanzas ~init:Package.Name.Map.empty
          ~f:(fun _ stanza acc ->
            let add_in_package_sites acc pkg site loc =
              let+ section = Sites.section_of_site sites ~loc ~pkg ~site in
              add_in_package_section acc pkg section
            in
            match stanza with
            | Dune_file.Install { section = Site { pkg; site; loc }; _ } ->
              add_in_package_sites acc pkg site loc
            | Dune_file.Plugin { site = loc, (pkg, site); _ } ->
              add_in_package_sites acc pkg site loc
            | _ -> Memo.return acc)
      in
      (* Add the site of the local package: it should only useful for making
         sure that at least one location is given to the site of local package
         because if the site is used it should already be in
         [packages_sections] *)
      Package.Name.Map.foldi packages ~init:package_sections
        ~f:(fun package_name (package : Package.t) acc ->
          Section.Site.Map.fold package.sites ~init:acc ~f:(fun section acc ->
              add_in_package_section acc package_name section))
    in
    let env_dune_dir_locations =
      let roots =
        Local_install_path.dir ~context:context.name
        |> Path.build |> Install.Section.Paths.Roots.opam_from_prefix
      in
      let init =
        match
          Stdune.Env.get context.env
            Dune_site_private.dune_dir_locations_env_var
        with
        | None -> []
        | Some var -> (
          match Dune_site_private.decode_dune_dir_locations var with
          | Some s -> s
          | None ->
            User_error.raise
              [ Pp.textf "Invalid env var %s=%S"
                  Dune_site_private.dune_dir_locations_env_var var
              ])
      in
      Package.Name.Map.foldi ~init package_sections
        ~f:(fun package_name sections init ->
          let paths = Install.Section.Paths.make ~package:package_name ~roots in
          Section.Set.fold sections ~init ~f:(fun section acc ->
              let package = Package.Name.to_string package_name in
              let dir =
                Path.to_absolute_filename
                  (Install.Section.Paths.get paths section)
              in
              { Dune_site_private.package; dir; section } :: acc))
    in
    if List.is_empty env_dune_dir_locations then context.env
    else
      Stdune.Env.add context.env
        ~var:Dune_site_private.dune_dir_locations_env_var
        ~value:
          (Dune_site_private.encode_dune_dir_locations env_dune_dir_locations)
  in
  (* Env node that represents the environment configured for the workspace. It
     is used as default at the root of every project in the workspace. *)
  let default_env =
    Memo.lazy_ (fun () ->
        let make ~inherit_from ~config_stanza =
          let dir = context.build_dir in
          let+ scope = Scope.DB.find_by_dir dir in
          let project = Scope.project scope in
          let default_context_flags = default_context_flags context ~project in
          let expander_for_artifacts =
            Memo.lazy_ (fun () ->
                Code_error.raise
                  "[expander_for_artifacts] in [default_env] is undefined" [])
          in
          let profile = context.profile in
          Dune_env.Stanza.fire_hooks config_stanza ~profile;
          let default_cxx_link_flags = Cxx_flags.get_flags ~for_:Link context in
          let expander = Memo.Lazy.of_val root_expander in
          Env_node.make ~dir ~scope ~inherit_from ~config_stanza ~profile
            ~expander ~expander_for_artifacts ~default_context_flags
            ~default_env:context_env ~default_bin_artifacts:artifacts.bin
            ~default_cxx_link_flags
        in
        make ~config_stanza:context.env_nodes.context
          ~inherit_from:
            (Some
               (Memo.lazy_ (fun () ->
                    make ~inherit_from:None
                      ~config_stanza:context.env_nodes.workspace))))
  in
  Env_tree.create ~context ~default_env ~host_env_tree:host ~root_expander
    ~bin_artifacts:artifacts.bin ~context_env

let all =
  Memo.lazy_ (fun () ->
      let open Memo.O in
      let* packages = Only_packages.get ()
      and* contexts = Context.DB.all () in
      let rec sctxs =
        lazy
          (Context_name.Map.of_list_map_exn contexts ~f:(fun (c : Context.t) ->
               (c.name, Memo.Lazy.create (fun () -> make_sctx c))))
      and make_sctx (context : Context.t) =
        let host () =
          match context.for_host with
          | None -> Memo.return None
          | Some h ->
            let+ sctx =
              Memo.Lazy.force
                (Context_name.Map.find_exn (Lazy.force sctxs) h.name)
            in
            Some sctx
        in
        let* host, stanzas =
          Memo.fork_and_join host (fun () ->
              Only_packages.filtered_stanzas context)
        in
        create ~host ~context ~packages ~stanzas
      in
      Lazy.force sctxs |> Context_name.Map.to_list
      |> Memo.parallel_map ~f:(fun (name, sctx) ->
             let+ sctx = Memo.Lazy.force sctx in
             (name, sctx))
      >>| Context_name.Map.of_list_exn)

let find name =
  let open Memo.O in
  let+ all = Memo.Lazy.force all in
  Context_name.Map.find all name

module As_memo_key = struct
  type nonrec t = t

  let equal = equal

  let hash = hash

  let to_dyn = to_dyn_concise

  module And_package = struct
    type nonrec t = t * Package.t

    let hash (x, y) = Poly.hash (hash x, Package.hash y)

    let equal (x1, y1) (x2, y2) = equal x1 x2 && y1 == y2

    let to_dyn (s, p) = Dyn.Tuple [ to_dyn s; Package.to_dyn p ]
  end
end
