open! Stdune
open Import

(* the parts of Super_context sufficient to construct env nodes *)
module Env_context = struct
  type data = (Path.Build.t, Env_node.t) Table.t

  type t =
    { env : data
    ; profile : Profile.t
    ; scopes : Scope.DB.t
    ; context_env : Env.t
    ; default_env : Env_node.t lazy_t
    ; stanzas_per_dir : Stanza.t list Dir_with_dune.t Path.Build.Map.t
    ; host : t option
    ; build_dir : Path.Build.t
    ; context : Context.t
    ; expander : Expander.t
    ; bin_artifacts : Artifacts.Bin.t
    }
end

type t =
  { context : Context.t
  ; scopes : Scope.DB.t
  ; public_libs : Lib.DB.t
  ; installed_libs : Lib.DB.t
  ; stanzas : Dune_file.Stanzas.t Dir_with_dune.t list
  ; stanzas_per_dir : Dune_file.Stanzas.t Dir_with_dune.t Path.Build.Map.t
  ; packages : Package.t Package.Name.Map.t
  ; file_tree : File_tree.t
  ; artifacts : Artifacts.t
  ; expander : Expander.t
  ; chdir : (Action.t, Action.t) Build.t
  ; host : t option
  ; libs_by_package : (Package.t * Lib.Local.Set.t) Package.Name.Map.t
  ; env_context : Env_context.t
  ; dir_status_db : Dir_status.DB.t
  ; external_lib_deps_mode : bool
  ; (* Env node that represent the environment configured for the workspace. It
    is used as default at the root of every project in the workspace. *)
    default_env : Env_node.t Lazy.t
  ; projects_by_key : Dune_project.t Dune_project.File_key.Map.t
  }

let context t = t.context

let stanzas t = t.stanzas

let stanzas_in t ~dir = Path.Build.Map.find t.stanzas_per_dir dir

let packages t = t.packages

let artifacts t = t.artifacts

let file_tree t = t.file_tree

let build_dir t = t.context.build_dir

let profile t = t.context.profile

let external_lib_deps_mode t = t.external_lib_deps_mode

let equal = (( == ) : t -> t -> bool)

let hash t = Context.hash t.context

let to_dyn_concise t = Context.to_dyn_concise t.context

let to_dyn t = Context.to_dyn t.context

let host t = Option.value t.host ~default:t

let libs_of_package t pkg_name =
  match Package.Name.Map.find t.libs_by_package pkg_name with
  | None -> Lib.Local.Set.empty
  | Some (_, libs) -> libs

let internal_lib_names t =
  List.fold_left t.stanzas ~init:Lib_name.Set.empty
    ~f:(fun acc { Dir_with_dune.data = stanzas; _ } ->
      List.fold_left stanzas ~init:acc ~f:(fun acc ->
        function
        | Dune_file.Library lib ->
          Lib_name.Set.add
            ( match lib.public with
            | None -> acc
            | Some { name = _, name; _ } -> Lib_name.Set.add acc name )
            (Lib_name.of_local lib.name)
        | _ -> acc))

let public_libs t = t.public_libs

let installed_libs t = t.installed_libs

let find_scope_by_dir t dir = Scope.DB.find_by_dir t.scopes dir

let find_scope_by_project t = Scope.DB.find_by_project t.scopes

let find_project_by_key t =
  Dune_project.File_key.Map.find_exn t.projects_by_key

module External_env = Env

module Env : sig
  type t = Env_context.t

  val ocaml_flags : t -> dir:Path.Build.t -> Ocaml_flags.t

  val c_flags :
    t -> dir:Path.Build.t -> (unit, string list) Build.t C.Kind.Dict.t

  val external_ : t -> dir:Path.Build.t -> External_env.t

  val bin_artifacts_host : t -> dir:Path.Build.t -> Artifacts.Bin.t

  val expander : t -> dir:Path.Build.t -> Expander.t

  val local_binaries : t -> dir:Path.Build.t -> File_binding.Expanded.t list
end = struct
  include Env_context

  let get_env_stanza t ~dir =
    Option.value ~default:Dune_env.Stanza.empty
    @@
    let open Option.O in
    let* stanza = Path.Build.Map.find t.stanzas_per_dir dir in
    List.find_map stanza.data ~f:(function
      | Dune_env.T config -> Some config
      | _ -> None)

  let rec get t ~dir ~scope =
    match Table.find t.env dir with
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
      Table.set t.env dir node;
      node

  let get t ~dir =
    match Table.find t.env dir with
    | Some node -> node
    | None -> (
      let scope = Scope.DB.find_by_dir t.scopes dir in
      try get t ~dir ~scope
      with Exit ->
        Code_error.raise "Super_context.Env.get called on invalid directory"
          [ ("dir", Path.Build.to_dyn dir) ] )

  let external_ t ~dir =
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

  let inline_tests ({ profile; _ } as t) ~dir =
    let node = get t ~dir in
    Env_node.inline_tests node ~profile

  let bin_artifacts t ~dir =
    let expander =
      expander_for_artifacts t ~context_expander:t.expander ~dir
    in
    Env_node.bin_artifacts (get t ~dir) ~profile:t.profile ~expander
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
    let bindings =
      let str =
        inline_tests t ~dir |> Dune_env.Stanza.Inline_tests.to_string
      in
      Pform.Map.singleton "inline_tests" (Values [ String str ])
    in
    expander
    |> Expander.add_bindings ~bindings
    |> Expander.set_bin_artifacts ~bin_artifacts_host

  let ocaml_flags t ~dir =
    Env_node.ocaml_flags (get t ~dir) ~profile:t.profile
      ~expander:(expander t ~dir)

  let default_context_flags (ctx : Context.t) =
    let c = ctx.ocamlc_cflags in
    let cxx =
      List.filter ctx.ocamlc_cflags ~f:(fun s ->
        not (String.is_prefix s ~prefix:"-std="))
    in
    C.Kind.Dict.make ~c ~cxx

  let c_flags t ~dir =
    let default_context_flags = default_context_flags t.context in
    Env_node.c_flags (get t ~dir) ~profile:t.profile
      ~expander:(expander t ~dir) ~default_context_flags
end

let expander t ~dir = Env.expander t.env_context ~dir

let add_rule t ?sandbox ?mode ?locks ?loc ~dir build =
  let build = Build.O.( >>> ) build t.chdir in
  let env = Env.external_ t.env_context ~dir in
  Rules.Produce.rule
    (Rule.make ?sandbox ?mode ?locks ~info:(Rule.Info.of_loc_opt loc)
      ~context:(Some t.context) ~env:(Some env) build)

let add_rule_get_targets t ?sandbox ?mode ?locks ?loc ~dir build =
  let build = Build.O.( >>> ) build t.chdir in
  let env = Env.external_ t.env_context ~dir in
  let rule =
    Rule.make ?sandbox ?mode ?locks ~info:(Rule.Info.of_loc_opt loc)
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

let partial_expand sctx ~dep_kind ~targets_written_by_user ~map_exe ~expander t
  =
  let acc = Expander.Resolved_forms.empty () in
  let c_flags ~dir = Env.c_flags sctx.env_context ~dir in
  let expander =
    Expander.with_record_deps expander acc ~dep_kind ~targets_written_by_user
      ~map_exe ~c_flags
  in
  let partial = Action_unexpanded.partial_expand t ~expander ~map_exe in
  (partial, acc)

let dir_is_vendored t src_dir =
  Option.value ~default:false (File_tree.dir_is_vendored t.file_tree src_dir)

let build_dir_is_vendored t build_dir =
  let opt =
    let open Option.O in
    let+ src_dir = Path.Build.drop_build_context build_dir in
    dir_is_vendored t src_dir
  in
  Option.value ~default:false opt

let ocaml_flags t ~dir (x : Dune_file.Buildable.t) =
  let expander = Env.expander t.env_context ~dir in
  let flags =
    Ocaml_flags.make ~spec:x.flags
      ~default:(Env.ocaml_flags t.env_context ~dir)
      ~eval:(Expander.expand_and_eval_set expander)
  in
  let dir_is_vendored = build_dir_is_vendored t dir in
  if dir_is_vendored then
    Ocaml_flags.with_vendored_warnings flags
  else
    flags

let c_flags t ~dir ~expander ~flags =
  let t = t.env_context in
  let ccg = Context.cc_g t.context in
  let default = Env.c_flags t ~dir in
  C.Kind.Dict.mapi flags ~f:(fun ~kind flags ->
    let name = C.Kind.to_string kind in
    Build.memoize (sprintf "%s flags" name)
      (let default = C.Kind.Dict.get default kind in
       let c = Expander.expand_and_eval_set expander flags ~standard:default in
       let open Build.O in
       c >>^ fun l -> l @ ccg))

let local_binaries t ~dir = Env.local_binaries t.env_context ~dir

let dump_env t ~dir =
  let t = t.env_context in
  let open Build.O in
  let o_dump = Ocaml_flags.dump (Env.ocaml_flags t ~dir) in
  let c_dump =
    let c_flags = Env.c_flags t ~dir in
    Build.fanout c_flags.c c_flags.cxx
    >>^ fun (c_flags, cxx_flags) ->
    List.map
      ~f:Dune_lang.Encoder.(pair string (list string))
      [ ("c_flags", c_flags); ("cxx_flags", cxx_flags) ]
  in
  (* combine o_dump and c_dump *)
  o_dump &&& c_dump >>^ fun (x, y) -> x @ y

let resolve_program t ~dir ?hint ~loc bin =
  let t = t.env_context in
  let bin_artifacts = Env.bin_artifacts_host t ~dir in
  Artifacts.Bin.binary ?hint ~loc bin_artifacts bin

let get_installed_binaries stanzas ~(context : Context.t) =
  let install_dir = Config.local_install_bin_dir ~context:context.name in
  let expander = Expander.expand_with_reduced_var_set ~context in
  let expand_str ~dir sw =
    let dir = Path.build dir in
    String_with_vars.expand ~dir ~mode:Single
      ~f:(fun var ver ->
        match expander var ver with
        | Unknown -> None
        | Expanded x -> Some x
        | Restricted ->
          User_error.raise
            ~loc:(String_with_vars.Var.loc var)
            [ Pp.textf "%s isn't allowed in this position."
              (String_with_vars.Var.describe var)
            ])
      sw
    |> Value.to_string ~dir
  in
  let expand_str_partial ~dir sw =
    String_with_vars.partial_expand ~dir ~mode:Single
      ~f:(fun var ver ->
        match expander var ver with
        | Expander.Unknown
         |Restricted ->
          None
        | Expanded x -> Some x)
      sw
    |> String_with_vars.Partial.map ~f:(Value.to_string ~dir)
  in
  Dir_with_dune.deep_fold stanzas ~init:Path.Build.Set.empty
    ~f:(fun d stanza acc ->
      let binaries_from_install files =
        List.fold_left files ~init:acc ~f:(fun acc fb ->
          let p =
            File_binding.Unexpanded.destination_relative_to_install_path fb
              ~section:Bin
              ~expand:(expand_str ~dir:d.ctx_dir)
              ~expand_partial:(expand_str_partial ~dir:(Path.build d.ctx_dir))
          in
          let p = Path.Local.of_string (Install.Dst.to_string p) in
          if Path.Local.is_root (Path.Local.parent_exn p) then
            Path.Build.Set.add acc (Path.Build.append_local install_dir p)
          else
            acc)
      in
      match (stanza : Stanza.t) with
      | Dune_file.Install { section = Bin; files; _ } ->
        binaries_from_install files
      | Dune_file.Executables
        ({ install_conf = Some { section = Bin; files; _ }; _ } as exes) ->
        let compile_info =
          Lib.DB.resolve_user_written_deps_for_exes (Scope.libs d.scope)
            exes.names exes.buildable.libraries
            ~pps:(Dune_file.Preprocess_map.pps exes.buildable.preprocess)
            ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
            ~variants:exes.variants ~optional:exes.optional
        in
        let available =
          Result.is_ok (Lib.Compile.direct_requires compile_info)
        in
        if available then
          binaries_from_install files
        else
          acc
      | _ -> acc)

let create ~(context : Context.t) ?host ~projects ~file_tree ~packages ~stanzas
  ~external_lib_deps_mode =
  let installed_libs =
    let stdlib_dir = context.stdlib_dir in
    Lib.DB.create_from_findlib context.findlib ~stdlib_dir
      ~external_lib_deps_mode
  in
  let scopes, public_libs =
    let libs, external_variants =
      Dune_load.Dune_file.fold_stanzas stanzas ~init:([], [])
        ~f:(fun dune_file stanza ((libs, external_variants) as acc) ->
          match stanza with
          | Dune_file.Library lib ->
            let ctx_dir =
              Path.Build.append_source context.build_dir dune_file.dir
            in
            ((ctx_dir, lib) :: libs, external_variants)
          | Dune_file.External_variant ev -> (libs, ev :: external_variants)
          | _ -> acc)
    in
    let lib_config = Context.lib_config context in
    Scope.DB.create ~projects ~context:context.name ~installed_libs ~lib_config
      libs external_variants
  in
  let stanzas =
    List.map stanzas
      ~f:(fun { Dune_load.Dune_file.dir; project; stanzas; kind = _ } ->
        let ctx_dir = Path.Build.append_source context.build_dir dir in
        let dune_version = Dune_project.dune_version project in
        { Dir_with_dune.src_dir = dir
        ; ctx_dir
        ; data = stanzas
        ; scope = Scope.DB.find_by_project scopes project
        ; dune_version
        })
  in
  let stanzas_per_dir =
    List.map stanzas ~f:(fun stanzas ->
      (stanzas.Dir_with_dune.ctx_dir, stanzas))
    |> Path.Build.Map.of_list_exn
  in
  let env = Table.create (module Path.Build) 128 in
  let default_env =
    lazy
      (let make ~inherit_from ~config =
        let dir = context.build_dir in
        Env_node.make ~dir
          ~scope:(Scope.DB.find_by_dir scopes dir)
          ~inherit_from ~config
       in
       make ~config:context.env_nodes.context
         ~inherit_from:
           (Some
             ( lazy
               (make ~inherit_from:None ~config:context.env_nodes.workspace) )))
  in
  let artifacts =
    let public_libs = ({ context; public_libs } : Artifacts.Public_libs.t) in
    { Artifacts.public_libs
    ; bin =
      Artifacts.Bin.create ~context
        ~local_bins:(get_installed_binaries ~context stanzas)
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
      ~context ~lib_artifacts:artifacts.public_libs
      ~bin_artifacts_host:artifacts_host.bin
  in
  let env_context =
    { Env_context.env
    ; profile = context.profile
    ; scopes
    ; context_env = context.env
    ; default_env
    ; stanzas_per_dir
    ; host = Option.map host ~f:(fun x -> x.env_context)
    ; build_dir = context.build_dir
    ; context
    ; expander
    ; bin_artifacts = artifacts.Artifacts.bin
    }
  in
  let dir_status_db = Dir_status.DB.make file_tree ~stanzas_per_dir in
  let projects_by_key =
    Dune_project.File_key.Map.of_list_map_exn projects ~f:(fun project ->
      (Dune_project.file_key project, project))
  in
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
  ; chdir =
    Build.arr (fun (action : Action.t) ->
      match action with
      | Chdir _ -> action
      | _ -> Chdir (Path.build context.build_dir, action))
  ; libs_by_package =
    Lib.DB.all public_libs |> Lib.Set.to_list
    |> List.filter_map ~f:(fun lib ->
      Lib.Local.of_lib lib
      |> Option.map ~f:(fun local ->
        (Option.value_exn (Lib.package lib), local)))
    |> Package.Name.Map.of_list_multi
    |> Package.Name.Map.merge packages ~f:(fun _name pkg libs ->
      let pkg = Option.value_exn pkg in
      let libs = Option.value libs ~default:[] in
      Some (pkg, Lib.Local.Set.of_list libs))
  ; env_context
  ; default_env
  ; external_lib_deps_mode
  ; dir_status_db
  ; projects_by_key
  }

module Libs = struct
  open Build.O

  let gen_select_rules t ~dir compile_info =
    List.iter (Lib.Compile.resolved_selects compile_info) ~f:(fun rs ->
      let { Lib.Compile.Resolved_select.dst_fn; src_fn } = rs in
      let dst = Path.Build.relative dir dst_fn in
      add_rule t ~dir
        ( match src_fn with
        | Ok src_fn ->
          let src = Path.build (Path.Build.relative dir src_fn) in
          Build.copy_and_add_line_directive ~src ~dst
        | Error e -> Build.fail ~targets:[ dst ] { fail = (fun () -> raise e) }
        ))

  let with_lib_deps t compile_info ~dir ~f =
    let prefix =
      Build.record_lib_deps (Lib.Compile.lib_deps_info compile_info)
    in
    let prefix =
      if t.context.merlin then
        Path.Build.relative dir ".merlin-exists"
        |> Path.build |> Build.path >>> prefix
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
    Expander.expand_path expander s |> Alias.of_user_written_path ~loc

  let dep t expander = function
    | File s ->
      let path = Expander.expand_path expander s in
      Build.path path >>^ fun () -> [ path ]
    | Alias s -> Build.alias (make_alias expander s) >>^ fun () -> []
    | Alias_rec s ->
      Build_system.Alias.dep_rec ~loc:(String_with_vars.loc s)
        ~file_tree:t.file_tree (make_alias expander s)
      >>^ fun () -> []
    | Glob_files s ->
      let loc = String_with_vars.loc s in
      let path = Expander.expand_path expander s in
      let pred = Glob.of_string_exn loc (Path.basename path) |> Glob.to_pred in
      let dir = Path.parent_exn path in
      File_selector.create ~dir pred
      |> Build.paths_matching ~loc >>^ Path.Set.to_list
    | Source_tree s ->
      let path = Expander.expand_path expander s in
      Build.source_tree ~dir:path ~file_tree:t.file_tree >>^ Path.Set.to_list
    | Package p ->
      let pkg = Package.Name.of_string (Expander.expand_str expander p) in
      Build.alias (Build_system.Alias.package_install ~context:t.context ~pkg)
      >>^ fun () -> []
    | Universe -> Build.dep Dep.universe >>^ fun () -> []
    | Env_var var_sw ->
      let var = Expander.expand_str expander var_sw in
      Build.env_var var >>^ fun () -> []
    | Sandbox_config sandbox_config ->
      Build.dep (Dep.sandbox_config sandbox_config) >>^ fun () -> []

  let make_interpreter ~f t ~expander l =
    let forms = Expander.Resolved_forms.empty () in
    let c_flags ~dir = Env.c_flags t.env_context ~dir in
    let expander =
      Expander.with_record_no_ddeps expander forms ~dep_kind:Optional
        ~map_exe:Fn.id ~c_flags
    in
    let deps = List.map l ~f:(f t expander) |> Build.all >>^ List.concat in
    Build.fanout4 deps
      (Build.record_lib_deps (Expander.Resolved_forms.lib_deps forms))
      (let ddeps = String.Map.to_list (Expander.Resolved_forms.ddeps forms) in
       Build.all (List.map ddeps ~f:snd))
      (Build.path_set (Expander.Resolved_forms.sdeps forms))
    >>^ fun (deps, _, _, _) -> deps

  let interpret t ~expander l =
    make_interpreter ~f:dep t ~expander l >>^ fun _paths -> ()

  let interpret_named =
    make_interpreter ~f:(fun t expander ->
      function
      | Bindings.Unnamed p ->
        dep t expander p
        >>^ fun l -> List.map l ~f:(fun x -> Bindings.Unnamed x)
      | Named (s, ps) ->
        Build.all (List.map ps ~f:(dep t expander))
        >>^ fun l -> [ Bindings.Named (s, List.concat l) ])
end

module Action = struct
  open Build.O
  module U = Action_unexpanded

  let map_exe sctx =
    match sctx.host with
    | None -> fun exe -> exe
    | Some host -> (
      fun exe ->
        match Path.extract_build_context_dir exe with
        | Some (dir, exe)
          when Path.equal dir (Path.build sctx.context.build_dir) ->
          Path.append_source (Path.build host.context.build_dir) exe
        | _ -> exe )

  let run sctx ~loc ~expander ~dep_kind ~targets:targets_written_by_user
    ~targets_dir t : (Path.t Bindings.t, Action.t) Build.t =
    let dir = Expander.dir expander in
    let map_exe = map_exe sctx in
    ( match (targets_written_by_user : Expander.Targets.t) with
    | Static _
     |Infer ->
      ()
    | Forbidden context -> (
      match U.Infer.unexpanded_targets t with
      | [] -> ()
      | x :: _ ->
        let loc = String_with_vars.loc x in
        User_error.raise ~loc
          [ Pp.textf "%s must not have targets." (String.capitalize context) ]
      ) );
    let t, forms =
      partial_expand sctx ~expander ~dep_kind ~targets_written_by_user ~map_exe
        t
    in
    let { U.Infer.Outcome.deps; targets } =
      match targets_written_by_user with
      | Infer -> U.Infer.partial t ~all_targets:true
      | Static { targets = targets_written_by_user; multiplicity = _ } ->
        let targets_written_by_user =
          Path.Build.Set.of_list targets_written_by_user
        in
        let { U.Infer.Outcome.deps; targets } =
          U.Infer.partial t ~all_targets:false
        in
        { deps
        ; targets = Path.Build.Set.union targets targets_written_by_user
        }
      | Forbidden _ ->
        let { U.Infer.Outcome.deps; targets = _ } =
          U.Infer.partial t ~all_targets:false
        in
        { U.Infer.Outcome.deps; targets = Path.Build.Set.empty }
    in
    let targets = Path.Build.Set.to_list targets in
    List.iter targets ~f:(fun target ->
      if Path.Build.( <> ) (Path.Build.parent_exn target) targets_dir then
        User_error.raise ~loc
          [ Pp.text
            "This action has targets in a different directory than the \
             current one, this is not allowed by dune at the moment:"
          ; Pp.enumerate targets ~f:(fun target ->
            Pp.text (Dpath.describe_path (Path.build target)))
          ]);
    let build =
      Build.record_lib_deps (Expander.Resolved_forms.lib_deps forms)
      >>> Build.path_set
        (Path.Set.union deps (Expander.Resolved_forms.sdeps forms))
      >>> Build.arr (fun paths -> ((), paths))
      >>>
      let ddeps = String.Map.to_list (Expander.Resolved_forms.ddeps forms) in
      Build.first (Build.all (List.map ddeps ~f:snd))
      >>^ (fun (vals, deps_written_by_user) ->
        let dynamic_expansions =
          List.fold_left2 ddeps vals ~init:String.Map.empty
            ~f:(fun acc (var, _) value -> String.Map.set acc var value)
        in
        let unresolved =
          let expander =
            Expander.add_ddeps_and_bindings expander ~dynamic_expansions
              ~deps_written_by_user
          in
          U.Partial.expand t ~expander ~map_exe
        in
        Action.Unresolved.resolve unresolved ~f:(fun loc prog ->
          match Expander.resolve_binary ~loc expander ~prog with
          | Ok path -> path
          | Error { fail } -> fail ()))
      >>> Build.dyn_path_set
        (Build.arr (fun action ->
          let { U.Infer.Outcome.deps; targets = _ } = U.Infer.infer action in
          deps))
      >>> Build.action_dyn () ~dir:(Path.build dir) ~targets
    in
    match Expander.Resolved_forms.failures forms with
    | [] -> build
    | fail :: _ -> Build.fail fail >>> build
end

let opaque t =
  Profile.is_dev t.context.profile
  && Ocaml_version.supports_opaque_for_mli t.context.version

let dir_status_db t = t.dir_status_db

module As_memo_key = struct
  type nonrec t = t

  let equal = equal

  let hash = hash

  let to_dyn = to_dyn_concise
end
