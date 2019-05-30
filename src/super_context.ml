open! Stdune
open Import

(* the parts of Super_context sufficient to construct env nodes *)
module Env_context = struct
  type data = (Path.Build.t, Env_node.t) Hashtbl.t
  type t = {
    env : data;
    profile : string;
    scopes : Scope.DB.t;
    context_env : Env.t;
    default_env : Env_node.t lazy_t;
    stanzas_per_dir : Stanza.t list Dir_with_dune.t Path.Build.Map.t;
    host : t option;
    build_dir : Path.Build.t;
    context : Context.t;
    expander : Expander.t;
    bin_artifacts : Artifacts.Bin.t;
  }
end

type t =
  { context                          : Context.t
  ; scopes                           : Scope.DB.t
  ; public_libs                      : Lib.DB.t
  ; installed_libs                   : Lib.DB.t
  ; stanzas                          : Dune_file.Stanzas.t Dir_with_dune.t list
  ; stanzas_per_dir                  : Dune_file.Stanzas.t Dir_with_dune.t Path.Build.Map.t
  ; packages                         : Package.t Package.Name.Map.t
  ; file_tree                        : File_tree.t
  ; artifacts                        : Artifacts.t
  ; expander                         : Expander.t
  ; chdir                            : (Action.t, Action.t) Build.t
  ; host                             : t option
  ; libs_by_package : (Package.t * Lib.Set.t) Package.Name.Map.t
  ; env_context                      : Env_context.t
  ; dir_status_db                    : Dir_status.DB.t
  ; external_lib_deps_mode           : bool
  ; (* Env node that represent the environment configured for the
       workspace. It is used as default at the root of every project
       in the workspace. *)
    default_env : Env_node.t Lazy.t
  }

let context t = t.context
let stanzas t = t.stanzas
let stanzas_in t ~dir = Path.Build.Map.find t.stanzas_per_dir dir
let packages t = t.packages
let libs_by_package t = t.libs_by_package
let artifacts t = t.artifacts
let file_tree t = t.file_tree
let build_dir t = t.context.build_dir
let profile t = t.context.profile
let external_lib_deps_mode t = t.external_lib_deps_mode

let equal = ((==) : t -> t -> bool)
let hash t = Context.hash t.context
let to_dyn t = Context.to_dyn t.context
let to_sexp t = Context.to_sexp t.context

let host t = Option.value t.host ~default:t

let internal_lib_names t =
  List.fold_left t.stanzas ~init:Lib_name.Set.empty
    ~f:(fun acc { Dir_with_dune. data = stanzas; _ } ->
      List.fold_left stanzas ~init:acc ~f:(fun acc -> function
        | Dune_file.Library lib ->
          Lib_name.Set.add
            (match lib.public with
             | None -> acc
             | Some { name = (_, name); _ } ->
               Lib_name.Set.add acc name)
            (Lib_name.of_local lib.name)
        | _ -> acc))

let public_libs    t = t.public_libs
let installed_libs t = t.installed_libs

let find_scope_by_dir t dir = Scope.DB.find_by_dir t.scopes dir
let find_scope_by_name t name = Scope.DB.find_by_name t.scopes name

module External_env = Env

module Env : sig
  type t = Env_context.t
  val ocaml_flags : t -> dir:Path.Build.t -> Ocaml_flags.t
  val c_flags : t -> dir:Path.Build.t -> (unit, string list) Build.t C.Kind.Dict.t
  val external_ : t -> dir:Path.Build.t -> External_env.t
  val bin_artifacts_host : t -> dir:Path.Build.t -> Artifacts.Bin.t
  val expander : t -> dir:Path.Build.t -> Expander.t
  val local_binaries : t -> dir:Path.Build.t -> File_binding.Expanded.t list
end = struct

  include Env_context

  let get_env_stanza t ~dir =
    let open Option.O in
    let* stanza = Path.Build.Map.find t.stanzas_per_dir dir in
    List.find_map stanza.data ~f:(function
      | Dune_env.T config -> Some config
      | _ -> None)

  let rec get t ~dir ~scope =
    match Hashtbl.find t.env dir with
    | Some node -> node
    | None ->
      let node =
        let inherit_from =
          if Path.Build.equal dir (Scope.root scope) then
            t.default_env
          else
            match Path.Build.parent dir with
            | None -> raise_notrace Exit
            | Some parent -> lazy (get t ~dir:parent ~scope)
        in
        let config = get_env_stanza t ~dir in
        Env_node.make ~dir ~scope ~config ~inherit_from:(Some inherit_from)
      in
      Hashtbl.add t.env dir node;
      node

  let get t ~dir =
    match Hashtbl.find t.env dir with
    | Some node -> node
    | None ->
      let scope =
        Scope.DB.find_by_dir t.scopes dir in
      try
        get t ~dir ~scope
      with Exit ->
        Exn.code_error "Super_context.Env.get called on invalid directory"
          [ "dir", Path.Build.to_sexp dir ]

  let external_ t  ~dir =
    Env_node.external_ (get t ~dir) ~profile:t.profile ~default:t.context_env

  let expander_for_artifacts t ~context_expander ~dir =
    let node = get t ~dir in
    let external_ = external_ t ~dir in
    Expander.extend_env context_expander ~env:external_
    |> Expander.set_scope ~scope:(Env_node.scope node)
    |> Expander.set_dir ~dir

  let local_binaries t ~dir =
    let node = get t ~dir in
    let expander =
      expander_for_artifacts ~context_expander:t.expander t ~dir
    in
    Env_node.local_binaries node ~profile:t.profile ~expander

  let bin_artifacts t ~dir =
    let expander =
      expander_for_artifacts t ~context_expander:t.expander ~dir
    in
    Env_node.bin_artifacts
      (get t ~dir) ~profile:t.profile
      ~expander
      ~default:t.bin_artifacts

  let bin_artifacts_host t ~dir =
    match t.host with
    | None -> bin_artifacts t ~dir
    | Some host ->
      let dir =
        Path.Build.drop_build_context_exn dir
        |> Path.Build.append_source host.build_dir
      in
      bin_artifacts host ~dir

  let expander t ~dir =
    let expander =
      expander_for_artifacts t ~context_expander:t.expander ~dir
    in
    let bin_artifacts_host = bin_artifacts_host t ~dir in
    Expander.set_bin_artifacts expander ~bin_artifacts_host

  let ocaml_flags t ~dir =
    Env_node.ocaml_flags (get t ~dir)
      ~profile:t.profile ~expander:(expander t ~dir)

  let default_context_flags (ctx : Context.t) =
    let c = ctx.ocamlc_cflags in
    let cxx =
        List.filter ctx.ocamlc_cflags
          ~f:(fun s -> not (String.is_prefix s ~prefix:"-std=")) in
    C.Kind.Dict.make ~c ~cxx

  let c_flags t ~dir =
    let default_context_flags = default_context_flags t.context in
    Env_node.c_flags (get t ~dir)
      ~profile:t.profile ~expander:(expander t ~dir)
      ~default_context_flags
end

let expander t ~dir = Env.expander t.env_context ~dir

let add_rule t ?sandbox ?mode ?locks ?loc ~dir build =
  let build = Build.O.(>>>) build t.chdir in
  let env = Env.external_ t.env_context ~dir in
  Rules.Produce.rule
    (Rule.make ?sandbox ?mode ?locks
       ~info:(Rule.Info.of_loc_opt loc)
       ~context:(Some t.context) ~env:(Some env) build)

let add_rule_get_targets t ?sandbox ?mode ?locks ?loc ~dir build =
  let build = Build.O.(>>>) build t.chdir in
  let env = Env.external_ t.env_context ~dir in
  let rule =
    Rule.make ?sandbox ?mode ?locks
      ~info:(Rule.Info.of_loc_opt loc)
      ~context:(Some t.context) ~env:(Some env) build
  in
  Rules.Produce.rule rule;
  rule.targets

let add_rules t ?sandbox ~dir builds =
  List.iter builds ~f:(add_rule t ?sandbox ~dir)

let add_alias_action t alias ~dir ~loc ?locks ~stamp action =
  let t = t.env_context in
  let env = Some (Env.external_ t ~dir) in
  Rules.Produce.Alias.add_action ~context:t.context ~env alias ~loc ?locks
    ~stamp action

let source_files t ~src_path =
  match File_tree.find_dir t.file_tree src_path with
  | None -> String.Set.empty
  | Some dir -> File_tree.Dir.files dir

module Pkg_version = struct
  open Build.O

  let file sctx (p : Package.t) =
    Path.Build.relative (Path.Build.append_source sctx.context.build_dir p.path)
      (sprintf "%s.version.sexp" (Package.Name.to_string p.name))

  let read_file fn =
    let fn = Path.build fn in
    Build.memoize "package version"
      (Build.contents fn
       >>^ fun s ->
       Dune_lang.Decoder.(parse (option string)) Univ_map.empty
         (Dune_lang.parse_string ~fname:(Path.to_string fn) ~mode:Single s))

  let read sctx p = read_file (file sctx p)

  let set sctx p get =
    let fn = file sctx p in
    add_rule sctx ~dir:(build_dir sctx)
      ((get >>^ fun v ->
        (Dune_lang.Encoder.(option string) v
         |> Dune_lang.to_string ~syntax:Dune))
       >>> Build.write_file_dyn fn);
    read_file fn
end

let partial_expand sctx ~dep_kind ~targets_written_by_user ~map_exe
      ~expander t =
  let acc = Expander.Resolved_forms.empty () in
  let read_package = Pkg_version.read sctx in
  let c_flags ~dir = Env.c_flags sctx.env_context ~dir in
  let expander =
    Expander.with_record_deps expander  acc ~dep_kind ~targets_written_by_user
      ~map_exe ~read_package ~c_flags in
  let partial = Action_unexpanded.partial_expand t ~expander ~map_exe in
  (partial, acc)

let ocaml_flags t ~dir (x : Dune_file.Buildable.t) =
  let t = t.env_context in
  let expander = Env.expander t ~dir in
  Ocaml_flags.make
    ~spec:x.flags
    ~default:(Env.ocaml_flags t ~dir)
    ~eval:(Expander.expand_and_eval_set expander)

let c_flags t ~dir ~expander ~flags =
  let t = t.env_context in
  let ccg = Context.cc_g t.context in
  let default = Env.c_flags t ~dir in
  C.Kind.Dict.mapi flags ~f:(fun ~kind flags ->
    let name = C.Kind.to_string kind in
    Build.memoize (sprintf "%s flags" name)
      begin
        let default = C.Kind.Dict.get default kind in
        let c = Expander.expand_and_eval_set expander
                  flags ~standard:default in
        let open Build.O in (c >>^ fun l -> l @ ccg)
      end)

let local_binaries t ~dir = Env.local_binaries t.env_context ~dir

let dump_env t ~dir =
  let t = t.env_context in
  let open Build.O in
  let o_dump = Ocaml_flags.dump (Env.ocaml_flags t ~dir) in
  let c_dump =
    let c_flags = Env.c_flags t ~dir in
    Build.fanout c_flags.c c_flags.cxx
    >>^ fun (c_flags, cxx_flags) ->
    List.map ~f:Dune_lang.Encoder.(pair string (list string))
      [ "c_flags", c_flags
      ; "cxx_flags", cxx_flags
      ]
  in (* combine o_dump and c_dump *)
  (o_dump &&& c_dump) >>^ (fun (x, y) -> x @ y)


let resolve_program t ~dir ?hint ~loc bin =
  let t = t.env_context in
  let bin_artifacts = Env.bin_artifacts_host t ~dir in
  Artifacts.Bin.binary ?hint ~loc bin_artifacts bin

let get_installed_binaries stanzas ~(context : Context.t) =
  let install_dir = Config.local_install_bin_dir ~context:context.name in
  let expander = Expander.expand_with_reduced_var_set ~context in
  let expand_str ~dir sw =
    let dir = Path.build dir in
    String_with_vars.expand ~dir ~mode:Single ~f:(fun var ver ->
      match expander var ver with
      | Unknown -> None
      | Expanded x -> Some x
      | Restricted ->
        Errors.fail (String_with_vars.Var.loc var)
          "%s isn't allowed in this position"
          (String_with_vars.Var.describe var)
    ) sw
    |> Value.to_string ~dir
  in
  let expand_str_partial ~dir sw =
    String_with_vars.partial_expand ~dir ~mode:Single ~f:(fun var ver ->
      match expander var ver with
      | Expander.Unknown | Restricted -> None
      | Expanded x -> Some x
    ) sw
    |> String_with_vars.Partial.map ~f:(Value.to_string ~dir)
  in
  Dir_with_dune.deep_fold stanzas ~init:Path.Build.Set.empty
    ~f:(fun d stanza acc ->
      match (stanza : Stanza.t) with
      | Dune_file.Install { section = Bin; files; _ } ->
        List.fold_left files ~init:acc ~f:(fun acc fb ->
          let p =
            File_binding.Unexpanded.destination_relative_to_install_path
              fb
              ~section:Bin
              ~expand:(expand_str ~dir:d.ctx_dir)
              ~expand_partial:(expand_str_partial ~dir:(Path.build d.ctx_dir))
          in
          let p = Path.Relative.of_string (Install.Dst.to_string p) in
          if Path.Relative.is_root (Path.Relative.parent_exn p) then
            Path.Build.Set.add acc (Path.Build.append_relative install_dir p)
          else
            acc)
      | _ -> acc)

let create
      ~(context:Context.t)
      ?host
      ~projects
     ~file_tree
      ~packages
      ~stanzas
      ~external_lib_deps_mode
  =
  let installed_libs =
    Lib.DB.create_from_findlib context.findlib ~external_lib_deps_mode
  in
  let internal_libs =
    List.concat_map stanzas
      ~f:(fun { Dune_load.Dune_file. dir; stanzas; project = _ ; kind = _ } ->
        let ctx_dir = Path.Build.append_source context.build_dir dir in
        List.filter_map stanzas ~f:(fun stanza ->
          match (stanza : Stanza.t) with
          | Dune_file.Library lib -> Some (ctx_dir, lib)
          | _ -> None))
  in
  let scopes, public_libs =
    let lib_config = Context.lib_config context in
    Scope.DB.create
      ~projects
      ~context:context.name
      ~installed_libs
      ~lib_config
      internal_libs
  in
  let stanzas =
    List.map stanzas
      ~f:(fun { Dune_load.Dune_file. dir; project; stanzas; kind } ->
        let ctx_dir = Path.Build.append_source context.build_dir dir in
        let dune_version = Dune_project.dune_version project in
        { Dir_with_dune.
          src_dir = dir
        ; ctx_dir
        ; data = stanzas
        ; scope = Scope.DB.find_by_name scopes (Dune_project.name project)
        ; kind
        ; dune_version
        })
  in
  let stanzas_per_dir =
    List.map stanzas ~f:(fun stanzas ->
      (stanzas.Dir_with_dune.ctx_dir, stanzas))
    |> Path.Build.Map.of_list_exn
  in
  let env = Hashtbl.create 128 in
  let default_env = lazy (
    let make ~inherit_from ~config =
      let dir = context.build_dir in
      Env_node.make
        ~dir
        ~scope:(Scope.DB.find_by_dir scopes dir)
        ~inherit_from
        ~config
    in
    match context.env_nodes with
    | { context = None; workspace = None } ->
      make ~config:(Some { loc = Loc.none; rules = [] }) ~inherit_from:None
    | { context = Some _ as config; workspace = None }
    | { context = None; workspace = Some _ as config } ->
      make ~config ~inherit_from:None
    | { context = Some _ as context ; workspace = Some _ as workspace } ->
      make ~config:context
        ~inherit_from:(Some (lazy (make ~inherit_from:None
                                     ~config:workspace)))
  )
  in
  let artifacts =
    let public_libs = ({
      context;
      public_libs
    } : Artifacts.Public_libs.t)
    in
    { Artifacts.public_libs;
      bin =
        Artifacts.Bin.create ~context
          ~local_bins:(
            get_installed_binaries
              ~context
              stanzas
          )
    }
  in
  let expander =
    let artifacts_host =
      match host with
      | None -> artifacts
      | Some host -> host.artifacts
    in
    Expander.make
      ~scope:(Scope.DB.find_by_dir scopes context.build_dir)
      ~context
      ~lib_artifacts:artifacts.public_libs
      ~bin_artifacts_host:artifacts_host.bin
  in
  let env_context = { Env_context.
    env;
    profile = context.profile;
    scopes;
    context_env = context.env;
    default_env;
    stanzas_per_dir;
    host = Option.map host ~f:(fun x -> x.env_context);
    build_dir = context.build_dir;
    context = context;
    expander = expander;
    bin_artifacts = artifacts.Artifacts.bin;
  }
  in
  let dir_status_db = Dir_status.DB.make file_tree ~stanzas_per_dir in
  { context
  ; expander
  ; host
  ; scopes
  ; public_libs
  ; installed_libs
  ; stanzas
  ; stanzas_per_dir
  ; packages
  ; file_tree
  ; artifacts
  ; chdir = Build.arr (fun (action : Action.t) ->
      match action with
      | Chdir _ -> action
      | _ -> Chdir (Path.build context.build_dir, action))
  ; libs_by_package =
      Lib.DB.all public_libs
      |> Lib.Set.to_list
      |> List.map ~f:(fun lib ->
        (Option.value_exn (Lib.package lib), lib))
      |> Package.Name.Map.of_list_multi
      |> Package.Name.Map.merge packages ~f:(fun _name pkg libs ->
        let pkg  = Option.value_exn pkg          in
        let libs = Option.value libs ~default:[] in
        Some (pkg, Lib.Set.of_list libs))
  ; env_context
  ; default_env
  ; external_lib_deps_mode
  ; dir_status_db
  }

module Libs = struct
  open Build.O

  let gen_select_rules t ~dir compile_info =
    List.iter (Lib.Compile.resolved_selects compile_info) ~f:(fun rs ->
      let { Lib.Compile.Resolved_select.dst_fn; src_fn } = rs in
      let dst = Path.Build.relative dir dst_fn in
      add_rule t
        ~dir
        (match src_fn with
         | Ok src_fn ->
           let src = Path.build (Path.Build.relative dir src_fn) in
           Build.copy_and_add_line_directive ~src ~dst
         | Error e ->
           Build.fail ~targets:[dst]
             { fail = fun () ->
                 raise (Lib.Error (No_solution_found_for_select e))
             }))

  let with_lib_deps t compile_info ~dir ~f =
    let prefix =
      Build.record_lib_deps (Lib.Compile.lib_deps_info compile_info)
    in
    let prefix =
      if t.context.merlin then
        Path.Build.relative dir ".merlin-exists"
        |> Path.build
        |> Build.path
        >>> prefix
      else
        prefix
    in
    Build_system.prefix_rules prefix ~f
end

module Deps = struct
  open Build.O
  open Dune_file.Dep_conf

  let make_alias expander s =
    let loc = String_with_vars.loc s in
    Expander.expand_path expander s
    |> Alias.of_user_written_path ~loc

  let dep t expander = function
    | File s ->
      let path = Expander.expand_path expander s in
      Build.path path
      >>^ fun () -> [path]
    | Alias s ->
      Build.alias (make_alias expander s)
      >>^ fun () -> []
    | Alias_rec s ->
      Build_system.Alias.dep_rec
        ~loc:(String_with_vars.loc s) ~file_tree:t.file_tree
        (make_alias expander s)
      >>^ fun () -> []
    | Glob_files s -> begin
        let loc = String_with_vars.loc s in
        let path = Expander.expand_path expander s in
        let pred =
          Glob.of_string_exn loc (Path.basename path)
          |> Glob.to_pred
        in
        let dir = Path.parent_exn path in
        File_selector.create ~dir pred
        |> Build.paths_matching ~loc
        >>^ Path.Set.to_list
      end
    | Source_tree s ->
      let path = Expander.expand_path expander s in
      Build.source_tree ~dir:path ~file_tree:t.file_tree
      >>^ Path.Set.to_list
    | Package p ->
      let pkg = Package.Name.of_string (Expander.expand_str expander p) in
      Build.alias (Build_system.Alias.package_install ~context:t.context ~pkg)
      >>^ fun () -> []
    | Universe ->
      Build.dep Dep.universe
      >>^ fun () -> []
    | Env_var var_sw ->
      let var = Expander.expand_str expander var_sw in
      Build.env_var var
      >>^ fun () -> []

  let make_interpreter ~f t ~expander l =
    let forms = Expander.Resolved_forms.empty () in
    let c_flags ~dir = Env.c_flags t.env_context ~dir in
    let expander =
      Expander.with_record_no_ddeps expander forms
        ~dep_kind:Optional ~map_exe:Fn.id ~c_flags
    in
    let deps =
      List.map l ~f:(f t expander)
      |> Build.all
      >>^ List.concat in
    Build.fanout4
      deps
      (Build.record_lib_deps (Expander.Resolved_forms.lib_deps forms))
      (let ddeps = String.Map.to_list (Expander.Resolved_forms.ddeps forms) in
       Build.all (List.map ddeps ~f:snd))
      (Build.path_set (Expander.Resolved_forms.sdeps forms))
    >>^ (fun (deps, _, _, _) -> deps)

  let interpret t ~expander l =
    make_interpreter ~f:dep t ~expander l
    >>^ (fun _paths -> ())

  let interpret_named =
    make_interpreter ~f:(fun t expander -> function
      | Bindings.Unnamed p ->
        dep t expander p >>^ fun l ->
        List.map l ~f:(fun x -> Bindings.Unnamed x)
      | Named (s, ps) ->
        Build.all (List.map ps ~f:(dep t expander)) >>^ fun l ->
        [Bindings.Named (s, List.concat l)])
end

module Scope_key = struct
  let of_string sctx key =
    match String.rsplit2 key ~on:'@' with
    | None ->
      (key, public_libs sctx)
    | Some (key, scope) ->
      ( key
      , Scope.libs (find_scope_by_name sctx
                      (Dune_project.Name.of_encoded_string scope)))

  let to_string key scope =
    sprintf "%s@%s" key (Dune_project.Name.to_encoded_string scope)
end

module Action = struct
  open Build.O
  module U = Action_unexpanded

  let map_exe sctx =
    match sctx.host with
    | None -> (fun exe -> exe)
    | Some host ->
      fun exe ->
        match Path.extract_build_context_dir exe with
        | Some (dir, exe)
          when Path.equal dir (Path.build sctx.context.build_dir) ->
          Path.append_source (Path.build host.context.build_dir) exe
        | _ -> exe

  let run sctx ~loc ~expander ~dep_kind ~targets:targets_written_by_user
        ~targets_dir t
    : (Path.t Bindings.t, Action.t) Build.t =
    let dir = Expander.dir expander in
    let map_exe = map_exe sctx in
    begin match (targets_written_by_user : Expander.Targets.t) with
    | Static _ | Infer -> ()
    | Forbidden context ->
      match U.Infer.unexpanded_targets t with
      | [] -> ()
      | x :: _ ->
        let loc = String_with_vars.loc x in
        Errors.warn loc
          "%s must not have targets, this target will be ignored.\n\
           This will become an error in the future."
          (String.capitalize context)
    end;
    let t, forms =
      partial_expand sctx ~expander ~dep_kind
        ~targets_written_by_user ~map_exe t
    in
    let { U.Infer.Outcome. deps; targets } =
      match targets_written_by_user with
      | Infer -> U.Infer.partial t ~all_targets:true
      | Static targets_written_by_user ->
        let targets_written_by_user = Path.Set.of_list targets_written_by_user in
        let { U.Infer.Outcome. deps; targets } =
          U.Infer.partial t ~all_targets:false
        in
        { deps; targets = Path.Set.union targets targets_written_by_user }
      | Forbidden _ ->
        let { U.Infer.Outcome. deps; targets = _ } =
          U.Infer.partial t ~all_targets:false
        in
        { deps; targets = Path.Set.empty }
    in
    let targets =
      Path.Set.fold ~init:[] targets
        ~f:(fun p acc -> Path.as_in_build_dir_exn p :: acc)
    in
    List.iter targets ~f:(fun target ->
      if Path.Build.(<>) (Path.Build.parent_exn target) targets_dir then
        Errors.fail loc
          "This action has targets in a different directory than the current \
           one, this is not allowed by dune at the moment:\n%s"
          (List.map targets ~f:(fun target ->
             sprintf "- %s" (Utils.describe_target (Path.build target)))
           |> String.concat ~sep:"\n"));
    let build =
      Build.record_lib_deps (Expander.Resolved_forms.lib_deps forms)
      >>>
      Build.path_set (Path.Set.union deps (Expander.Resolved_forms.sdeps forms))
      >>>
      Build.arr (fun paths -> ((), paths))
      >>>
      let ddeps = String.Map.to_list (Expander.Resolved_forms.ddeps forms) in
      Build.first (Build.all (List.map ddeps ~f:snd))
      >>^ (fun (vals, deps_written_by_user) ->
        let dynamic_expansions =
          List.fold_left2 ddeps vals ~init:String.Map.empty
            ~f:(fun acc (var, _) value -> String.Map.add acc var value)
        in
        let unresolved =
          let expander =
            Expander.add_ddeps_and_bindings expander ~dynamic_expansions
              ~deps_written_by_user in
          U.Partial.expand t ~expander ~map_exe
        in
        Action.Unresolved.resolve unresolved ~f:(fun loc prog ->
          match Expander.resolve_binary ~loc expander ~prog with
          | Ok path    -> path
          | Error { fail } -> fail ()))
      >>>
      Build.dyn_path_set (Build.arr (fun action ->
        let { U.Infer.Outcome.deps; targets = _ } =
          U.Infer.infer action
        in
        deps))
      >>>
      Build.action_dyn () ~dir:(Path.build dir) ~targets
    in
    match Expander.Resolved_forms.failures forms with
    | [] -> build
    | fail :: _ -> Build.fail fail >>> build
end

let opaque t =
  t.context.profile = "dev"
  && Ocaml_version.supports_opaque_for_mli t.context.version

let dir_status_db t = t.dir_status_db
