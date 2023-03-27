open Import

module Includes = struct
  type t = Command.Args.without_targets Command.Args.t Lib_mode.Cm_kind.Map.t

  let patched = true

  let filter_with_odeps libs deps md =
    let open Resolve.Memo.O in
    let+ module_deps, _ = deps in
    let external_dep_names =
      List.filter_map ~f:Module_dep.filter_external module_deps
      |> List.map ~f:Module_dep.External_name.to_string
    in
    (* Find a more general way to compare [ocamldep] output to Lib_name (?) *)
    List.filter libs ~f:(fun lib ->
        if Lib.is_local lib then (
          let lib_name =
            Lib.name lib |> Lib_name.to_string |> String.capitalize
          in
          Dune_util.Log.info
            [ Pp.textf "For module %s\n"
                (Module.name md |> Module_name.to_string)
            ];
          let exists_in_odeps lib_name =
            List.exists external_dep_names ~f:(fun odep ->
                Dune_util.Log.info
                  [ Pp.textf "Comparing %s %s \n" lib_name odep ];
                String.equal lib_name odep
                || String.is_prefix ~prefix:odep lib_name)
          in
          let exists = exists_in_odeps lib_name in
          let exists2 =
            match String.split ~on:'.' lib_name with
            | t ->
              List.exists t ~f:(fun lib_name ->
                  exists_in_odeps (String.capitalize lib_name))
          in
          if not exists then
            Dune_util.Log.info [ Pp.textf "False for full name %s \n" lib_name ];
          if not exists2 then
            Dune_util.Log.info [ Pp.textf "False for split %s \n" lib_name ];
          (* Replace '.' by '_' *)
          let lib_name_undescore =
            String.extract_words
              ~is_word_char:(fun c -> not (Char.equal c '.'))
              lib_name
            |> String.concat ~sep:"_"
          in
          let exists3 = exists_in_odeps lib_name_undescore in
          if not exists3 then
            Dune_util.Log.info
              [ Pp.textf "False for custom name, replacing . with _ %s \n"
                  lib_name_undescore
              ];
          (* (let local_name =
               match
                 Lib_name.to_local (Lib.info lib |> Lib_info.loc, Lib.name lib)
               with
               | Ok libname -> Lib_name.Local.to_string libname
               | Error e -> User_message.to_string e
             in
             Dune_util.Log.info [ Pp.textf "Local_name :  %s \n" local_name ]); *)
          let keep = exists || exists2 || exists3 in
          if not keep then
            Dune_util.Log.info [ Pp.textf "Removing %s \n" lib_name ];
          keep)
        else true)

  let make ~project ~opaque ~requires ~md ~dep_graphs =
    let open Lib_mode.Cm_kind.Map in
    let open Resolve.Memo.O in
    let iflags libs mode = Lib_flags.L.include_flags ~project libs mode in
    let deps =
      let dep_graph_impl = Ml_kind.Dict.get dep_graphs Ml_kind.Impl in
      let dep_graph_intf = Ml_kind.Dict.get dep_graphs Ml_kind.Intf in
      let module_deps_impl = Dep_graph.deps_of dep_graph_impl md in
      let module_deps_intf = Dep_graph.deps_of dep_graph_intf md in
      Action_builder.run
        (Action_builder.map2 module_deps_impl module_deps_intf
           ~f:(fun inft impl -> List.append inft impl))
        Action_builder.Eager
      |> Resolve.Memo.lift_memo
    in
    let make_includes_args ~mode groups =
      Command.Args.memo
        (Resolve.Memo.args
           (let* libs = requires in
            let+ libs' = filter_with_odeps libs deps md in
            let libs = if patched then libs' else libs in
            Command.Args.S
              [ iflags libs mode
              ; Hidden_deps (Lib_file_deps.deps libs ~groups)
              ]))
    in
    let cmi_includes = make_includes_args ~mode:(Ocaml Byte) [ Ocaml Cmi ] in
    let cmx_includes =
      Command.Args.memo
        (Resolve.Memo.args
           (let* libs = requires in
            let+ libs' = filter_with_odeps libs deps md in
            let libs = if patched then libs' else libs in
            Command.Args.S
              [ iflags libs (Ocaml Native)
              ; Hidden_deps
                  (if opaque then
                   List.map libs ~f:(fun lib ->
                       ( lib
                       , if Lib.is_local lib then
                           [ Lib_file_deps.Group.Ocaml Cmi ]
                         else [ Ocaml Cmi; Ocaml Cmx ] ))
                   |> Lib_file_deps.deps_with_exts
                  else
                    Lib_file_deps.deps libs
                      ~groups:[ Lib_file_deps.Group.Ocaml Cmi; Ocaml Cmx ])
              ]))
    in
    let melange_cmi_includes =
      make_includes_args ~mode:Melange [ Melange Cmi ]
    in
    let melange_cmj_includes =
      make_includes_args ~mode:Melange [ Melange Cmi; Melange Cmj ]
    in
    { ocaml = { cmi = cmi_includes; cmo = cmi_includes; cmx = cmx_includes }
    ; melange = { cmi = melange_cmi_includes; cmj = melange_cmj_includes }
    }

  let empty = Lib_mode.Cm_kind.Map.make_all Command.Args.empty
end

type opaque =
  | Explicit of bool
  | Inherit_from_settings

let eval_opaque (context : Context.t) = function
  | Explicit b -> b
  | Inherit_from_settings ->
    Profile.is_dev context.profile
    && Ocaml.Version.supports_opaque_for_mli context.version

type modules =
  { modules : Modules.t
  ; dep_graphs : Dep_graph.t Ml_kind.Dict.t
  }

let singleton_modules m =
  { modules = Modules.singleton m; dep_graphs = Dep_graph.Ml_kind.dummy m }

type t =
  { super_context : Super_context.t
  ; scope : Scope.t
  ; expander : Expander.t
  ; obj_dir : Path.Build.t Obj_dir.t
  ; modules : modules
  ; flags : Ocaml_flags.t
  ; requires_compile : Lib.t list Resolve.Memo.t
  ; requires_link : Lib.t list Resolve.t Memo.Lazy.t
  ; includes : md:Module.t -> Includes.t
  ; preprocessing : Pp_spec.t
  ; opaque : bool
  ; stdlib : Ocaml_stdlib.t option
  ; js_of_ocaml : Js_of_ocaml.In_context.t option
  ; sandbox : Sandbox_config.t
  ; package : Package.t option
  ; vimpl : Vimpl.t option
  ; public_lib_name : Lib_name.t option
  ; modes : Lib_mode.Map.Set.t
  ; bin_annot : bool
  ; ocamldep_modules_data : Ocamldep.Modules_data.t
  ; loc : Loc.t option
  }

let loc t = t.loc

let super_context t = t.super_context

let scope t = t.scope

let expander t = t.expander

let dir t = Obj_dir.dir t.obj_dir

let obj_dir t = t.obj_dir

let modules t = t.modules.modules

let flags t = t.flags

let requires_compile t = t.requires_compile

let requires_link t = Memo.Lazy.force t.requires_link

let includes t = t.includes

let preprocessing t = t.preprocessing

let opaque t = t.opaque

let stdlib t = t.stdlib

let js_of_ocaml t = t.js_of_ocaml

let sandbox t = t.sandbox

let set_sandbox t sandbox = { t with sandbox }

let package t = t.package

let public_lib_name t = t.public_lib_name

let vimpl t = t.vimpl

let modes t = t.modes

let bin_annot t = t.bin_annot

let context t = Super_context.context t.super_context

let ocamldep_modules_data t = t.ocamldep_modules_data

let dep_graphs t = t.modules.dep_graphs

let create ~super_context ~scope ~expander ~obj_dir ~modules ~flags
    ~requires_compile ~requires_link ?(preprocessing = Pp_spec.dummy) ~opaque
    ?stdlib ~js_of_ocaml ~package ?public_lib_name ?vimpl ?modes ?bin_annot ?loc
    () =
  let open Memo.O in
  let project = Scope.project scope in
  let requires_compile =
    if Dune_project.implicit_transitive_deps project then
      Memo.Lazy.force requires_link
    else requires_compile
  in
  let sandbox =
    (* With sandboxing, there are a few build errors in ocaml platform 1162238ae
       like: File "ocaml_modules/ocamlgraph/src/pack.ml", line 1: Error: The
       implementation ocaml_modules/ocamlgraph/src/pack.ml does not match the
       interface
       ocaml_modules/ocamlgraph/src/.graph.objs/byte/graph__Pack.cmi: *)
    Sandbox_config.no_sandboxing
  in
  let modes =
    let default =
      { Lib_mode.Map.ocaml =
          Mode.Dict.make_both (Some Dune_file.Mode_conf.Kind.Inherited)
      ; melange = None
      }
    in
    Option.value ~default modes |> Lib_mode.Map.map ~f:Option.is_some
  in
  let opaque = eval_opaque (Super_context.context super_context) opaque in
  let ocamldep_modules_data : Ocamldep.Modules_data.t =
    { dir = Obj_dir.dir obj_dir
    ; sandbox = Sandbox_config.no_special_requirements
    ; obj_dir
    ; sctx = super_context
    ; vimpl
    ; modules
    ; stdlib
    }
  in
  let+ dep_graphs = Dep_rules.rules ocamldep_modules_data
  and+ bin_annot =
    match bin_annot with
    | Some b -> Memo.return b
    | None -> Super_context.bin_annot super_context ~dir:(Obj_dir.dir obj_dir)
  in
  let includes =
    Includes.make ~project ~opaque ~requires:requires_compile ~dep_graphs
  in
  { super_context
  ; scope
  ; expander
  ; obj_dir
  ; modules = { modules; dep_graphs }
  ; flags
  ; requires_compile
  ; requires_link
  ; includes
  ; preprocessing
  ; opaque
  ; stdlib
  ; js_of_ocaml
  ; sandbox
  ; package
  ; vimpl
  ; public_lib_name
  ; modes
  ; bin_annot
  ; ocamldep_modules_data
  ; loc
  }

let for_alias_module t alias_module =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = (Super_context.context t.super_context).profile in
    Ocaml_flags.default ~dune_version ~profile
  in
  let sandbox =
    let ctx = Super_context.context t.super_context in
    (* If the compiler reads the cmi for module alias even with [-w -49
       -no-alias-deps], we must sandbox the build of the alias module since the
       modules it references are built after. *)
    if Ocaml.Version.always_reads_alias_cmi ctx.version then
      Sandbox_config.needs_sandboxing
    else Sandbox_config.no_special_requirements
  in
  let (modules, includes) : modules * (md:Module.t -> Includes.t) =
    match Modules.is_stdlib_alias t.modules.modules alias_module with
    | false -> (singleton_modules alias_module, fun ~md:_ -> Includes.empty)
    | true ->
      (* The stdlib alias module is different from the alias modules usually
         produced by Dune: it contains code and depends on a few other
         [CamlinnternalXXX] modules from the stdlib, so we need the full set of
         modules to compile it. *)
      (t.modules, t.includes)
  in
  { t with
    flags =
      Ocaml_flags.append_common flags
        [ "-w"; "-49"; "-nopervasives"; "-nostdlib" ]
  ; includes
  ; stdlib = None
  ; sandbox
  ; modules
  }

let for_root_module t root_module =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = (Super_context.context t.super_context).profile in
    Ocaml_flags.default ~profile ~dune_version
  in
  { t with
    flags =
      Ocaml_flags.append_common flags
        [ "-w"; "-49"; "-nopervasives"; "-nostdlib" ]
  ; stdlib = None
  ; modules = singleton_modules root_module
  }

let for_module_generated_at_link_time cctx ~requires ~module_ =
  let opaque =
    (* Cmi's of link time generated modules are compiled with -opaque, hence
       their implementation must also be compiled with -opaque *)
    let ctx = Super_context.context cctx.super_context in
    Ocaml.Version.supports_opaque_for_mli ctx.version
  in
  let modules = singleton_modules module_ in
  let dummy =
    Dep_graph.make ~dir:(Path.Build.of_string "")
      ~per_module:Module_name.Unique.Map.empty
  in
  let dep_graphs = Ml_kind.Dict.make ~intf:dummy ~impl:dummy in
  let includes =
    Includes.make ~dep_graphs ~project:(Scope.project cctx.scope) ~opaque
      ~requires
  in
  { cctx with
    opaque
  ; flags = Ocaml_flags.empty
  ; requires_link = Memo.lazy_ (fun () -> requires)
  ; requires_compile = requires
  ; includes
  ; modules
  }

let for_wrapped_compat t =
  { t with includes = (fun ~md:_ -> Includes.empty); stdlib = None }

let for_plugin_executable t ~embed_in_plugin_libraries =
  let libs = Scope.libs t.scope in
  let requires_link =
    Memo.lazy_ (fun () ->
        Resolve.Memo.List.map ~f:(Lib.DB.resolve libs) embed_in_plugin_libraries)
  in
  { t with requires_link }

let without_bin_annot t = { t with bin_annot = false }

let entry_module_names sctx t =
  match Lib_info.entry_modules (Lib.info t) with
  | External d -> Resolve.Memo.of_result d
  | Local ->
    let open Memo.O in
    let+ modules = Dir_contents.modules_of_lib sctx t in
    let modules = Option.value_exn modules in
    Resolve.return (Modules.entry_modules modules |> List.map ~f:Module.name)

let root_module_entries t =
  let open Action_builder.O in
  let* requires = Resolve.Memo.read t.requires_compile in
  let* l =
    Action_builder.List.map requires ~f:(fun lib ->
        Action_builder.of_memo (entry_module_names t.super_context lib)
        >>= Resolve.read)
  in
  Action_builder.return (List.concat l)

let set_obj_dir t obj_dir = { t with obj_dir }

let set_modes t ~modes = { t with modes }
