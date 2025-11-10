open Import
open Memo.O

module Includes = struct
  type ocaml =
    { cmi : Command.Args.without_targets Command.Args.t
    ; cmo : Command.Args.without_targets Command.Args.t
    ; cmx : Command.Args.without_targets Command.Args.t
    }

  type t =
    { ocaml : ocaml
    ; melange : Command.Args.without_targets Command.Args.t Melange.Cm_kind.Map.t
    }

  let make ~project ~opaque ~direct_requires ~hidden_requires lib_config : t =
    (* TODO: some of the requires can filtered out using [ocamldep] info *)
    let open Resolve.Memo.O in
    let iflags direct_libs hidden_libs mode =
      Lib_flags.L.include_flags ~project ~direct_libs ~hidden_libs mode lib_config
    in
    let make_includes_args ~mode groups =
      (let+ direct_libs = direct_requires
       and+ hidden_libs = hidden_requires in
       Command.Args.S
         [ iflags direct_libs hidden_libs mode
         ; Hidden_deps (Lib_file_deps.deps (direct_libs @ hidden_libs) ~groups)
         ])
      |> Resolve.Memo.args
      |> Command.Args.memo
    in
    { ocaml =
        (let cmi_includes = make_includes_args ~mode:(Ocaml Byte) [ Ocaml Cmi ] in
         { cmi = cmi_includes
         ; cmo = cmi_includes
         ; cmx =
             (let+ direct_libs = direct_requires
              and+ hidden_libs = hidden_requires in
              Command.Args.S
                [ iflags direct_libs hidden_libs (Ocaml Native)
                ; Hidden_deps
                    (let libs = direct_libs @ hidden_libs in
                     if opaque
                     then
                       List.map libs ~f:(fun lib ->
                         ( lib
                         , if Lib.is_local lib
                           then [ Lib_file_deps.Group.Ocaml Cmi ]
                           else [ Ocaml Cmi; Ocaml Cmx ] ))
                       |> Lib_file_deps.deps_with_exts
                     else
                       Lib_file_deps.deps
                         libs
                         ~groups:[ Lib_file_deps.Group.Ocaml Cmi; Ocaml Cmx ])
                ])
             |> Resolve.Memo.args
             |> Command.Args.memo
         })
    ; melange =
        { cmi = make_includes_args ~mode:Melange [ Melange Cmi ]
        ; cmj = make_includes_args ~mode:Melange [ Melange Cmi; Melange Cmj ]
        }
    }
  ;;

  let empty =
    { ocaml =
        { cmi = Command.Args.empty; cmo = Command.Args.empty; cmx = Command.Args.empty }
    ; melange = Melange.Cm_kind.Map.make_all Command.Args.empty
    }
  ;;
end

type opaque =
  | Explicit of bool
  | Inherit_from_settings

let eval_opaque (ocaml : Ocaml_toolchain.t) profile = function
  | Explicit b -> b
  | Inherit_from_settings ->
    Profile.is_dev profile && Ocaml.Version.supports_opaque_for_mli ocaml.version
;;

type modules =
  { modules : Modules.With_vlib.t
  ; dep_graphs : Dep_graph.t Ml_kind.Dict.t
  }

let singleton_modules m =
  { modules = Modules.With_vlib.singleton m; dep_graphs = Dep_graph.Ml_kind.dummy m }
;;

type t =
  { super_context : Super_context.t
  ; scope : Scope.t
  ; obj_dir : Path.Build.t Obj_dir.t
  ; modules : modules
  ; flags : Ocaml_flags.t
  ; requires_compile : Lib.t list Resolve.Memo.t
  ; requires_hidden : Lib.t list Resolve.Memo.t
  ; requires_link : Lib.t list Resolve.t Memo.Lazy.t
  ; implements : Virtual_rules.t
  ; parameters : Module_name.t list Resolve.Memo.t
  ; includes : Includes.t
  ; preprocessing : Pp_spec.t
  ; opaque : bool
  ; stdlib : Ocaml_stdlib.t option
  ; js_of_ocaml : Js_of_ocaml.In_context.t option Js_of_ocaml.Mode.Pair.t
  ; sandbox : Sandbox_config.t
  ; package : Package.t option
  ; melange_package_name : Lib_name.t option
  ; modes : Lib_mode.Map.Set.t
  ; bin_annot : bool
  ; loc : Loc.t option
  ; ocaml : Ocaml_toolchain.t
  }

let loc t = t.loc
let super_context t = t.super_context
let scope t = t.scope
let dir t = Obj_dir.dir t.obj_dir
let obj_dir t = t.obj_dir
let modules t = t.modules.modules
let flags t = t.flags
let requires_compile t = t.requires_compile
let requires_hidden t = t.requires_hidden
let requires_link t = Memo.Lazy.force t.requires_link
let parameters t = t.parameters
let includes t = t.includes
let preprocessing t = t.preprocessing
let opaque t = t.opaque
let stdlib t = t.stdlib
let js_of_ocaml t = t.js_of_ocaml
let sandbox t = t.sandbox
let set_sandbox t sandbox = { t with sandbox }
let package t = t.package
let melange_package_name t = t.melange_package_name
let implements t = t.implements
let modes t = t.modes
let bin_annot t = t.bin_annot
let context t = Super_context.context t.super_context
let dep_graphs t = t.modules.dep_graphs
let ocaml t = t.ocaml

let parameters_main_modules parameters =
  let open Resolve.Memo.O in
  let* parameters = parameters in
  Resolve.Memo.List.map parameters ~f:(fun param ->
    let+ main = Lib.main_module_name param in
    match main with
    | Some main -> main
    | None ->
      Code_error.raise
        "Expected library parameter to have a main module"
        [ "param", Lib.to_dyn param ])
;;

let create
      ~super_context
      ~scope
      ~obj_dir
      ~modules
      ~flags
      ~requires_compile
      ~requires_link
      ?(preprocessing = Pp_spec.dummy)
      ~opaque
      ?stdlib
      ~js_of_ocaml
      ~package
      ~melange_package_name
      ?(implements = Virtual_rules.no_implements)
      ?parameters
      ?modes
      ?bin_annot
      ?loc
      ()
  =
  let project = Scope.project scope in
  let context = Super_context.context super_context in
  let* ocaml = Context.ocaml context in
  let direct_requires, hidden_requires =
    match Dune_project.implicit_transitive_deps project ocaml.version with
    | Enabled -> Memo.Lazy.force requires_link, Resolve.Memo.return []
    | Disabled -> requires_compile, Resolve.Memo.return []
    | Disabled_with_hidden_includes ->
      let requires_hidden =
        let open Resolve.Memo.O in
        let+ requires_compile = requires_compile
        and+ requires_link = Memo.Lazy.force requires_link in
        let requires_table = Table.create (module Lib) 5 in
        List.iter ~f:(fun lib -> Table.set requires_table lib ()) requires_compile;
        List.filter requires_link ~f:(fun l -> not (Table.mem requires_table l))
      in
      requires_compile, requires_hidden
  in
  let parameters =
    match parameters with
    | None -> Resolve.Memo.return []
    | Some parameters -> parameters_main_modules parameters
  in
  let sandbox = Sandbox_config.no_special_requirements in
  let modes =
    let default =
      { Lib_mode.Map.ocaml = Mode.Dict.make_both (Some Mode_conf.Kind.Inherited)
      ; melange = None
      }
    in
    Option.value ~default modes |> Lib_mode.Map.map ~f:Option.is_some
  in
  let opaque =
    let profile = Context.profile context in
    eval_opaque ocaml profile opaque
  in
  let+ dep_graphs =
    Dep_rules.rules
      ~dir:(Obj_dir.dir obj_dir)
      ~sandbox
      ~obj_dir
      ~sctx:super_context
      ~impl:implements
      ~modules
  and+ bin_annot =
    match bin_annot with
    | Some b -> Memo.return b
    | None -> Env_stanza_db.bin_annot ~dir:(Obj_dir.dir obj_dir)
  in
  { super_context
  ; scope
  ; obj_dir
  ; modules = { modules; dep_graphs }
  ; flags
  ; requires_compile = direct_requires
  ; requires_hidden = hidden_requires
  ; requires_link
  ; implements
  ; parameters
  ; includes =
      Includes.make ~project ~opaque ~direct_requires ~hidden_requires ocaml.lib_config
  ; preprocessing
  ; opaque
  ; stdlib
  ; js_of_ocaml
  ; sandbox
  ; package
  ; melange_package_name
  ; modes
  ; bin_annot
  ; loc
  ; ocaml
  }
;;

let alias_and_root_module_flags =
  let extra = [ "-w"; "-49"; "-nopervasives"; "-nostdlib" ] in
  fun base -> Ocaml_flags.append_common base extra
;;

let for_alias_module t alias_module =
  let keep_flags = Modules.With_vlib.is_stdlib_alias (modules t) alias_module in
  let flags =
    if keep_flags
    then (* in the case of stdlib, these flags can be written by the user *)
      t.flags
    else (
      let project = Scope.project t.scope in
      let dune_version = Dune_project.dune_version project in
      let profile = Super_context.context t.super_context |> Context.profile in
      Ocaml_flags.default ~dune_version ~profile)
  in
  let sandbox =
    (* If the compiler reads the cmi for module alias even with [-w -49
       -no-alias-deps], we must sandbox the build of the alias module since the
       modules it references are built after. *)
    if Ocaml.Version.always_reads_alias_cmi t.ocaml.version
    then Sandbox_config.needs_sandboxing
    else Sandbox_config.no_special_requirements
  in
  let (modules, includes) : modules * Includes.t =
    match Modules.With_vlib.is_stdlib_alias t.modules.modules alias_module with
    | false -> singleton_modules alias_module, Includes.empty
    | true ->
      (* The stdlib alias module is different from the alias modules usually
         produced by Dune: it contains code and depends on a few other
         [CamlinnternalXXX] modules from the stdlib, so we need the full set of
         modules to compile it. *)
      t.modules, t.includes
  in
  { t with
    flags = alias_and_root_module_flags flags
  ; includes
  ; stdlib = None
  ; sandbox
  ; modules
  }
;;

let for_root_module t root_module =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = Super_context.context t.super_context |> Context.profile in
    Ocaml_flags.default ~profile ~dune_version
  in
  { t with
    flags = alias_and_root_module_flags flags
  ; stdlib = None
  ; modules = singleton_modules root_module
  }
;;

let for_module_generated_at_link_time cctx ~requires ~module_ =
  let opaque =
    (* Cmi's of link time generated modules are compiled with -opaque, hence
       their implementation must also be compiled with -opaque *)
    Ocaml.Version.supports_opaque_for_mli cctx.ocaml.version
  in
  let modules = singleton_modules module_ in
  let includes =
    let hidden_requires = Resolve.Memo.return [] in
    let direct_requires = requires in
    Includes.make
      ~project:(Scope.project cctx.scope)
      ~opaque
      ~direct_requires
      ~hidden_requires
      cctx.ocaml.lib_config
  in
  { cctx with
    opaque
  ; flags = Ocaml_flags.empty
  ; requires_link = Memo.lazy_ (fun () -> requires)
  ; requires_compile = requires
  ; includes
  ; modules
  }
;;

let for_wrapped_compat t =
  (* See #10689 *)
  let flags = Ocaml_flags.append_common t.flags [ "-w"; "-53" ] in
  { t with includes = Includes.empty; stdlib = None; flags }
;;

let for_plugin_executable t ~embed_in_plugin_libraries =
  let libs = Scope.libs t.scope in
  let requires_link =
    Memo.lazy_ (fun () ->
      Resolve.Memo.List.map ~f:(Lib.DB.resolve libs) embed_in_plugin_libraries)
  in
  { t with requires_link }
;;

let without_bin_annot t = { t with bin_annot = false }
let set_obj_dir t obj_dir = { t with obj_dir }
let set_modes t ~modes = { t with modes }
