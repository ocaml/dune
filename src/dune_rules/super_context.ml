open Import
open Memo.O

type t =
  { context : Context.t
  ; context_env : Env.t Memo.t (** context env with additional variables *)
  ; default_env : Env_node.t Memo.Lazy.t
  ; host : t option
  ; root_expander : Expander.t
  ; artifacts : Artifacts.t Memo.t
  ; get_node : Path.Build.t -> Env_node.t Memo.t
  }

let hash t = Context.hash t.context
let equal : t -> t -> bool = phys_equal
let to_dyn_concise t = Context.to_dyn_concise t.context
let to_dyn t = Context.to_dyn t.context

let set_context context path =
  let path = Path.Build.drop_build_context_exn path in
  Path.Build.append_source (Context.build_dir context) path
;;

let artifacts_host t ~dir =
  let t, dir =
    match t.host with
    | None -> t, dir
    | Some host -> host, set_context host.context dir
  in
  t.get_node dir >>= Env_node.artifacts
;;

let scope_host ~scope (context : Context.t) =
  match Context.for_host context with
  | None -> scope
  | Some host ->
    let* dir =
      let* scope = scope in
      let+ host = host in
      set_context host (Scope.root scope)
    in
    Scope.DB.find_by_dir dir
;;

let expander_for_artifacts t ~dir =
  let external_env = t.get_node dir >>= Env_node.external_env in
  let scope = Scope.DB.find_by_dir dir in
  let scope_host = scope_host ~scope t.context in
  let+ project = Dune_load.find_project ~dir in
  Expander.extend_env t.root_expander ~env:external_env
  |> Expander.set_scope ~dir ~project ~scope ~scope_host
;;

let expander t ~dir =
  let+ expander_for_artifacts = expander_for_artifacts t ~dir in
  let artifacts_host = artifacts_host t ~dir in
  Expander.set_artifacts expander_for_artifacts ~artifacts_host
;;

let get_env_stanza ~dir =
  let open Memo.O in
  Dune_load.stanzas_in_dir dir
  >>= (function
         | None -> Memo.return None
         | Some dune_file ->
           Dune_file.find_stanzas dune_file Dune_env.key
           >>| (function
            | [] -> None
            | [ x ] -> Some x
            | _ :: _ -> assert false))
  >>| Option.value ~default:Dune_env.empty
;;

let get_impl t dir =
  let inherit_from =
    Memo.lazy_ (fun () ->
      let* scope_root = Dune_load.find_project ~dir >>| Dune_project.root in
      if Path.Source.equal (Path.Build.drop_build_context_exn dir) scope_root
      then Memo.Lazy.force t.default_env
      else (
        match Path.Build.parent dir with
        | Some parent -> t.get_node parent
        | None ->
          Code_error.raise
            "Super_context.Env.get called on invalid directory"
            [ "dir", Path.Build.to_dyn dir ]))
  in
  let+ config_stanza = get_env_stanza ~dir in
  let expander =
    Memo.lazy_ (fun () -> expander_for_artifacts t ~dir) |> Memo.Lazy.force
  in
  let profile = Context.profile t.context in
  Env_node.make
    ~dir
    ~config_stanza
    ~inherit_from:(Some inherit_from)
    ~profile
    ~expander
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

open Memo.O

let extend_action t ~dir action =
  let open Action_builder.O in
  let+ (action : Action.Full.t) = action
  and+ env =
    Action_builder.of_memo
      (let open Memo.O in
       t.get_node dir >>= Env_node.external_env)
  in
  Action.Full.add_env env action
  |> Action.Full.map ~f:(function
    | Chdir _ as a -> a
    | a -> Chdir (Path.build (Context.build_dir t.context), a))
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

let resolve_program_memo t ~dir ?where ?hint ~loc bin =
  let* artifacts = artifacts_host t ~dir in
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
  ~artifacts
  =
  let make ~inherit_from ~config_stanza =
    let config_stanza = Option.value config_stanza ~default:Dune_env.empty in
    let dir = context.build_dir in
    let expander =
      let* () = Memo.return () in
      Code_error.raise "[expander_for_artifacts] in [default_env] is undefined" []
    in
    Dune_env.fire_hooks config_stanza ~profile;
    Env_node.make
      ~dir
      ~inherit_from
      ~config_stanza
      ~profile
      ~expander
      ~default_env:root_env
      ~default_artifacts:artifacts
  in
  make
    ~config_stanza:env_nodes.context
    ~inherit_from:
      (Some
         (Memo.lazy_ (fun () ->
            make ~inherit_from:None ~config_stanza:env_nodes.workspace |> Memo.return)))
;;

let make_root_env (context : Context.t) ~(host : t option) : Env.t Memo.t =
  let open Memo.O in
  let* env =
    let roots =
      let context = Context.name context in
      Install.Context.dir ~context |> Install.Roots.make ~relative:Path.Build.relative
    in
    Context.installed_env context >>| Install.Roots.add_to_env roots
  in
  let+ host_context, _PATH =
    let _PATH = Env.get env Env_path.var in
    match host with
    | None -> Memo.return (context, _PATH)
    | Some host ->
      let context = host.context in
      let+ _PATH =
        let+ env = Context.installed_env context in
        Env.get env Env_path.var
      in
      context, _PATH
  in
  Env.add
    env
    ~var:Env_path.var
    ~value:
      (Install.Context.bin_dir ~context:(Context.name host_context)
       |> Path.build
       |> Bin.cons_path ~_PATH)
;;

let create ~(context : Context.t) ~(host : t option) ~packages ~stanzas =
  let context_name = Context.name context in
  let env =
    let* base = make_root_env context ~host in
    Site_env.add_packages_env context_name ~base stanzas packages
  in
  let artifacts = Artifacts_db.get context in
  let+ root_expander =
    let public_libs = Scope.DB.public_libs context_name in
    let artifacts_host, public_libs_host, context_host =
      match Context.for_host context with
      | None -> artifacts, public_libs, Memo.return context
      | Some host ->
        let artifacts = host >>= Artifacts_db.get in
        let public_libs = host >>| Context.name >>= Scope.DB.public_libs in
        artifacts, public_libs, host
    in
    let scope = Scope.DB.find_by_dir (Context.build_dir context) in
    let scope_host = context_host >>| Context.build_dir >>= Scope.DB.find_by_dir in
    let+ project = Dune_load.find_project ~dir:(Context.build_dir context) in
    Expander.make_root
      ~project
      ~scope
      ~scope_host
      ~context
      ~env
      ~public_libs
      ~artifacts_host
      ~public_libs_host
  in
  (* Env node that represents the environment configured for the workspace. It
     is used as default at the root of every project in the workspace. *)
  let default_env =
    Memo.lazy_ ~name:"default_env" (fun () ->
      make_default_env_node
        (Context.build_context context)
        (Context.profile context)
        (Context.env_nodes context)
        ~root_env:env
        ~artifacts
      |> Memo.return)
  in
  create
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
    let* packages = Dune_load.packages ()
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
            h
            >>| Context.name
            >>| Context_name.Map.find_exn (Lazy.force sctxs)
            >>= Memo.Lazy.force
          in
          Some sctx
      in
      let* host, stanzas =
        Memo.fork_and_join host (fun () -> Dune_load.dune_files (Context.name context))
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
  Memo.Lazy.force all
  >>| Context_name.Map.values
  >>= Memo.parallel_iter ~f:(fun t -> t.artifacts >>= Artifacts.force)
;;

module As_memo_key = struct
  type nonrec t = t

  let equal = equal
  let hash = hash
  let to_dyn = to_dyn_concise

  module And_package_name = struct
    type nonrec t = t * Package.Name.t

    let hash = Tuple.T2.hash hash Package.Name.hash
    let equal (x1, y1) (x2, y2) = equal x1 x2 && Package.Name.equal y1 y2
    let to_dyn (s, p) = Dyn.Tuple [ to_dyn s; Package.Name.to_dyn p ]
  end
end

let () =
  Expander0.set_db (fun ~dir ->
    Context.DB.by_dir dir
    >>| Context.name
    >>= find_exn
    >>= expander ~dir
    >>| Expander.to_expander0)
;;

let context t = t.context
let env_node t ~dir = t.get_node dir
let context_env t = t.context_env
