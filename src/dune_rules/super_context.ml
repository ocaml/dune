open Import

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
      let+ inline_tests = Env_stanza_db.inline_tests ~dir in
      let str = Dune_env.Inline_tests.to_string inline_tests in
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
    extend_expander t ~dir ~expander_for_artifacts
  ;;

  let get_env_stanza ~dir =
    let open Memo.O in
    let+ stanzas = Only_packages.stanzas_in_dir dir in
    Option.value ~default:Dune_env.empty
    @@
    let open Option.O in
    let* stanzas = stanzas in
    List.find_map stanzas.stanzas ~f:(fun stanza ->
      match Stanza.repr stanza with
      | Dune_env.T config -> Some config
      | _ -> None)
  ;;

  let get_impl t dir =
    let* scope = Scope.DB.find_by_dir dir in
    let inherit_from =
      if Path.Build.equal dir (Scope.root scope)
      then Memo.lazy_ (fun () -> Memo.Lazy.force t.default_env)
      else (
        match Path.Build.parent dir with
        | None ->
          Code_error.raise
            "Super_context.Env.get called on invalid directory"
            [ "dir", Path.Build.to_dyn dir ]
        | Some parent -> Memo.lazy_ (fun () -> get_node t ~dir:parent))
    in
    let+ config_stanza = get_env_stanza ~dir in
    let expander_for_artifacts =
      Memo.lazy_ (fun () ->
        let* external_env = external_env t ~dir in
        expander_for_artifacts ~scope ~root_expander:t.root_expander ~external_env ~dir)
    in
    let profile = Context.profile t.context in
    Env_node.make
      ~dir
      ~scope
      ~config_stanza
      ~inherit_from:(Some inherit_from)
      ~profile
      ~expander_for_artifacts
      ~default_env:t.context_env
      ~default_artifacts:t.artifacts
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

let resolve_program_memo t ~dir ?where ?hint ~loc bin =
  let* artifacts = Env_tree.artifacts_host t ~dir in
  Artifacts.binary ?hint ?where ~loc artifacts bin
;;

let resolve_program t ~dir ?where ?hint ~loc bin =
  Action_builder.of_memo @@ resolve_program_memo t ~dir ?where ?hint ~loc bin
;;

let make_default_env_node
  (context : Build_context.t)
  profile
  (env_nodes : Context.Env_nodes.t)
  ~root_env
  ~(artifacts : Artifacts.t)
  =
  let make ~inherit_from ~config_stanza =
    let config_stanza = Option.value config_stanza ~default:Dune_env.empty in
    let dir = context.build_dir in
    let+ scope = Scope.DB.find_by_dir dir in
    let expander_for_artifacts =
      Memo.lazy_ (fun () ->
        Code_error.raise "[expander_for_artifacts] in [default_env] is undefined" [])
    in
    Dune_env.fire_hooks config_stanza ~profile;
    Env_node.make
      ~dir
      ~scope
      ~inherit_from
      ~config_stanza
      ~profile
      ~expander_for_artifacts
      ~default_env:root_env
      ~default_artifacts:artifacts
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

let create ~(context : Context.t) ~(host : t option) ~packages ~stanzas =
  let* env =
    let base = make_root_env context ~host in
    Site_env.add_packages_env (Context.name context) ~base stanzas packages
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
      ~env
      ~lib_artifacts:public_libs
      ~artifacts_host
      ~lib_artifacts_host:public_libs_host
  and+ artifacts = artifacts in
  (* Env node that represents the environment configured for the workspace. It
     is used as default at the root of every project in the workspace. *)
  let default_env =
    let profile = Context.profile context in
    Memo.lazy_ ~name:"default_env" (fun () ->
      make_default_env_node
        (Context.build_context context)
        profile
        (Context.env_nodes context)
        ~root_env:env
        ~artifacts)
  in
  Env_tree.create
    ~context
    ~default_env
    ~host_env_tree:host
    ~root_expander
    ~artifacts
    ~context_env:env
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
