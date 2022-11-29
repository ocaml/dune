open Import
module CC = Compilation_context

(* Arguments for the compiler to prevent it from being too clever.

   The compiler creates the cmi when it thinks a .ml file has no corresponding
   .mli. However this behavior is a bit racy and doesn't work well when the
   extension is not .ml or when the .ml and .mli are in different directories.
   This flags makes the compiler think there is a .mli file and will the read
   the cmi file rather than create it. *)
let force_read_cmi source_file = [ "-intf-suffix"; Path.extension source_file ]

(* Build the cm* if the corresponding source is present, in the case of cmi if
   the mli is not present it is added as additional target to the .cmo
   generation *)

let open_modules modules m =
  match Modules.alias_for modules m with
  | None -> []
  | Some (m : Module.t) -> [ Module.name m ]

let opens modules m =
  match open_modules modules m with
  | [] -> Command.Args.empty
  | modules ->
    Command.Args.S
      (List.map modules ~f:(fun name ->
           Command.Args.As [ "-open"; Module_name.to_string name ]))

let other_cm_files ~opaque ~cm_kind ~obj_dir =
  List.concat_map ~f:(fun m ->
      let cmi_kind = Lib_mode.Cm_kind.cmi cm_kind in
      let deps =
        [ Path.build (Obj_dir.Module.cm_file_exn obj_dir m ~kind:cmi_kind) ]
      in
      if Module.has m ~ml_kind:Impl && cm_kind = Ocaml Cmx && not opaque then
        let cmx = Obj_dir.Module.cm_file_exn obj_dir m ~kind:(Ocaml Cmx) in
        Path.build cmx :: deps
      else if Module.has m ~ml_kind:Impl && cm_kind = Melange Cmj then
        let cmj = Obj_dir.Module.cm_file_exn obj_dir m ~kind:(Melange Cmj) in
        Path.build cmj :: deps
      else deps)

let copy_interface ~sctx ~dir ~obj_dir ~cm_kind m =
  (* symlink the .cmi into the public interface directory *)
  Memo.when_
    (Module.visibility m <> Visibility.Private
    && Obj_dir.need_dedicated_public_dir obj_dir)
    (fun () ->
      let cmi_kind = Lib_mode.Cm_kind.cmi cm_kind in
      Super_context.add_rule sctx ~dir
        (Action_builder.symlink
           ~src:
             (Path.build (Obj_dir.Module.cm_file_exn obj_dir m ~kind:cmi_kind))
           ~dst:(Obj_dir.Module.cm_public_file_exn obj_dir m ~kind:cmi_kind)))

let melange_args ~package_output (cm_kind : Lib_mode.Cm_kind.t) package module_
    =
  match cm_kind with
  | Ocaml (Cmi | Cmo | Cmx) | Melange Cmi -> []
  | Melange Cmj ->
    let pkg_name_args =
      match package with
      | None -> []
      | Some pkg ->
        [ Command.Args.A "--bs-package-name"
        ; A (Package.Name.to_string (Package.name pkg))
        ]
    in
    Command.Args.A "--bs-stop-after-cmj" :: A "--bs-package-output"
    :: Command.Args.Path (Path.build package_output)
    :: A "--bs-module-name"
    :: A (Melange.js_basename module_)
    :: pkg_name_args

let build_cm cctx ~force_write_cmi ~precompiled_cmi ~cm_kind (m : Module.t)
    ~(phase : Fdo.phase option) =
  if force_write_cmi && precompiled_cmi then
    Code_error.raise "force_read_cmi and precompiled_cmi are mutually exclusive"
      [];
  let sctx = CC.super_context cctx in
  let dir = CC.dir cctx in
  let obj_dir = CC.obj_dir cctx in
  let ctx = Super_context.context sctx in
  let mode = Lib_mode.of_cm_kind cm_kind in
  let sandbox =
    let default = CC.sandbox cctx in
    match Module.kind m with
    | Root ->
      (* This is need to guarantee that no local modules shadow the modules
         referenced by the root module *)
      Sandbox_config.needs_sandboxing
    | _ -> default
  in
  let open Memo.O in
  let* compiler =
    match mode with
    | Melange ->
      let loc = CC.loc cctx in
      let+ melc = Melange_binary.melc sctx ~loc ~dir in
      Some melc
    | Ocaml mode ->
      Memo.return
        (let compiler = Context.compiler ctx mode in
         (* TODO one day remove this silly optimization *)
         match compiler with
         | Ok _ as s -> Some s
         | Error _ -> None)
  in
  (let open Option.O in
  let* compiler = compiler in
  let ml_kind = Lib_mode.Cm_kind.source cm_kind in
  let+ src = Module.file m ~ml_kind in
  let dst = Obj_dir.Module.cm_file_exn obj_dir m ~kind:cm_kind in
  let obj =
    Obj_dir.Module.obj_file obj_dir m ~kind:(Ocaml Cmx)
      ~ext:ctx.lib_config.ext_obj
  in
  let open Memo.O in
  let* extra_args, extra_deps, other_targets =
    if precompiled_cmi then Memo.return (force_read_cmi src, [], [])
    else
      (* If we're compiling an implementation, then the cmi is present *)
      let public_vlib_module = Module.kind m = Impl_vmodule in
      match phase with
      | Some Emit -> Memo.return ([], [], [])
      | Some Compile | Some All | None -> (
        match (cm_kind, Module.file m ~ml_kind:Intf, public_vlib_module) with
        (* If there is no mli, [ocamlY -c file.ml] produces both the .cmY and
           .cmi. We choose to use ocamlc to produce the cmi and to produce the
           cmx we have to wait to avoid race conditions. *)
        | (Ocaml Cmo | Melange Cmj), None, false ->
          if force_write_cmi then
            Memo.return ([ "-intf-suffix"; ".dummy-ignore-mli" ], [], [])
          else
            let+ () = copy_interface ~dir ~obj_dir ~sctx ~cm_kind m in
            let cmi_kind = Lib_mode.Cm_kind.cmi cm_kind in
            ([], [], [ Obj_dir.Module.cm_file_exn obj_dir m ~kind:cmi_kind ])
        | (Ocaml Cmo | Melange Cmj), None, true
        | (Ocaml (Cmo | Cmx) | Melange Cmj), _, _ ->
          let cmi_kind = Lib_mode.Cm_kind.cmi cm_kind in
          Memo.return
            ( force_read_cmi src
            , [ Path.build (Obj_dir.Module.cm_file_exn obj_dir m ~kind:cmi_kind)
              ]
            , [] )
        | (Ocaml Cmi | Melange Cmi), _, _ ->
          let+ () = copy_interface ~dir ~obj_dir ~sctx ~cm_kind m in
          ([], [], []))
  in
  let other_targets =
    match cm_kind with
    | Ocaml (Cmi | Cmo) | Melange (Cmi | Cmj) -> other_targets
    | Ocaml Cmx -> (
      match phase with
      | Some Compile ->
        let linear =
          Obj_dir.Module.obj_file obj_dir m ~kind:(Ocaml Cmx)
            ~ext:Fdo.linear_ext
        in
        linear :: other_targets
      | Some Emit -> other_targets
      | Some All | None -> obj :: other_targets)
  in
  let opaque = CC.opaque cctx in
  let other_cm_files =
    let dep_graph =
      Ml_kind.Dict.get (Compilation_context.dep_graphs cctx) ml_kind
    in
    let module_deps = Dep_graph.deps_of dep_graph m in
    Action_builder.dyn_paths_unit
      (Action_builder.map module_deps
         ~f:(other_cm_files ~opaque ~cm_kind ~obj_dir))
  in
  let other_targets, cmt_args =
    match cm_kind with
    | Ocaml Cmx -> (other_targets, Command.Args.empty)
    | Ocaml (Cmi | Cmo) | Melange (Cmi | Cmj) ->
      if Compilation_context.bin_annot cctx then
        let fn =
          Option.value_exn (Obj_dir.Module.cmt_file obj_dir m ~cm_kind ~ml_kind)
        in
        (fn :: other_targets, A "-bin-annot")
      else (other_targets, Command.Args.empty)
  in
  let opaque_arg : _ Command.Args.t =
    let intf_only = cm_kind = Ocaml Cmi && not (Module.has m ~ml_kind:Impl) in
    if opaque || (intf_only && Ocaml.Version.supports_opaque_for_mli ctx.version)
    then A "-opaque"
    else Command.Args.empty
  in
  let flags, sandbox =
    let flags =
      Ocaml_flags.get (CC.flags cctx)
        (match mode with
        | Ocaml m -> m
        | Melange ->
          (* TODO: define Melange default flags somewhere, should melange rules
             read from [flags] stanza as well? *)
          Byte)
    in
    match Module.pp_flags m with
    | None -> (flags, sandbox)
    | Some (pp, sandbox') ->
      ( (let open Action_builder.O in
        let+ flags = flags
        and+ pp_flags = pp in
        flags @ pp_flags)
      , Sandbox_config.inter sandbox sandbox' )
  in
  let output =
    match phase with
    | Some Compile -> dst
    | Some Emit -> obj
    | Some All | None -> dst
  in
  let src =
    match phase with
    | Some Emit ->
      let linear_fdo =
        Obj_dir.Module.obj_file obj_dir m ~kind:(Ocaml Cmx)
          ~ext:Fdo.linear_fdo_ext
      in
      Path.build linear_fdo
    | Some Compile | Some All | None -> src
  in
  let opens =
    let modules = Compilation_context.modules cctx in
    opens modules m
  in
  let obj_dirs =
    Obj_dir.all_obj_dirs obj_dir ~mode
    |> List.concat_map ~f:(fun p ->
           [ Command.Args.A "-I"; Path (Path.build p) ])
  in
  Super_context.add_rule sctx
    ~dir:
      (let dune_version =
         Compilation_context.scope cctx
         |> Scope.project |> Dune_project.dune_version
       in
       (* TODO DUNE4 get rid of the old behavior *)
       if dune_version >= (3, 7) then dir else ctx.build_dir)
    ?loc:(CC.loc cctx)
    (let open Action_builder.With_targets.O in
    Action_builder.with_no_targets (Action_builder.paths extra_deps)
    >>> Action_builder.with_no_targets other_cm_files
    >>> Command.run ~dir:(Path.build ctx.build_dir) compiler
          [ Command.Args.dyn flags
          ; cmt_args
          ; Command.Args.S obj_dirs
          ; Command.Args.as_any
              (Lib_mode.Cm_kind.Map.get (CC.includes cctx) cm_kind)
          ; As extra_args
          ; S
              (melange_args cm_kind
                 (Compilation_context.package cctx)
                 ~package_output:dir m)
          ; A "-no-alias-deps"
          ; opaque_arg
          ; As (Fdo.phase_flags phase)
          ; opens
          ; As
              (match Compilation_context.stdlib cctx with
              | None -> []
              | Some _ ->
                (* XXX why aren't these just normal library flags? *)
                [ "-nopervasives"; "-nostdlib" ])
          ; A "-o"
          ; Target output
          ; A "-c"
          ; Command.Ml_kind.flag ml_kind
          ; Dep src
          ; Hidden_targets other_targets
          ]
    >>| Action.Full.add_sandbox sandbox))
  |> Memo.Option.iter ~f:Fun.id

let build_module ?(force_write_cmi = false) ?(precompiled_cmi = false) cctx m =
  let open Memo.O in
  let { Lib_mode.Map.ocaml; melange } = Compilation_context.modes cctx in
  let build_cm = build_cm cctx m ~force_write_cmi ~precompiled_cmi in
  let* () =
    Memo.when_ (ocaml.byte || ocaml.native) (fun () ->
        let* () = build_cm ~cm_kind:(Ocaml Cmo) ~phase:None
        and* () =
          let ctx = CC.context cctx in
          let can_split =
            Ocaml.Version.supports_split_at_emit ctx.version
            || Ocaml_config.is_dev_version ctx.ocaml_config
          in
          match (ctx.fdo_target_exe, can_split) with
          | None, _ -> build_cm ~cm_kind:(Ocaml Cmx) ~phase:None
          | Some _, false -> build_cm ~cm_kind:(Ocaml Cmx) ~phase:(Some All)
          | Some _, true ->
            build_cm ~cm_kind:(Ocaml Cmx) ~phase:(Some Compile)
            >>> Fdo.opt_rule cctx m
            >>> build_cm ~cm_kind:(Ocaml Cmx) ~phase:(Some Emit)
        and* () =
          Memo.when_ (not precompiled_cmi) (fun () ->
              build_cm ~cm_kind:(Ocaml Cmi) ~phase:None)
        in
        let obj_dir = CC.obj_dir cctx in
        match Obj_dir.Module.cm_file obj_dir m ~kind:(Ocaml Cmo) with
        | None -> Memo.return ()
        | Some src ->
          Compilation_context.js_of_ocaml cctx
          |> Memo.Option.iter ~f:(fun in_context ->
                 (* Build *.cmo.js *)
                 let sctx = CC.super_context cctx in
                 let dir = CC.dir cctx in
                 let target = Path.Build.extend_basename src ~suffix:".js" in
                 let action_with_targets =
                   Jsoo_rules.build_cm cctx ~in_context ~src ~target
                 in
                 action_with_targets >>= Super_context.add_rule sctx ~dir))
  in
  Memo.when_ melange (fun () ->
      let* () = build_cm ~cm_kind:(Melange Cmj) ~phase:None in
      Memo.when_ (not precompiled_cmi) (fun () ->
          build_cm ~cm_kind:(Melange Cmi) ~phase:None))

let ocamlc_i ~deps cctx (m : Module.t) ~output =
  let sctx = CC.super_context cctx in
  let obj_dir = CC.obj_dir cctx in
  let dir = CC.dir cctx in
  let ctx = Super_context.context sctx in
  let src = Option.value_exn (Module.file m ~ml_kind:Impl) in
  let sandbox = Compilation_context.sandbox cctx in
  let cm_deps =
    Action_builder.dyn_paths_unit
      (let open Action_builder.O in
      let+ deps = Ml_kind.Dict.get deps Impl in
      List.concat_map deps ~f:(fun m ->
          [ Path.build (Obj_dir.Module.cm_file_exn obj_dir m ~kind:(Ocaml Cmi))
          ]))
  in
  let ocaml_flags = Ocaml_flags.get (CC.flags cctx) Mode.Byte in
  let modules = Compilation_context.modules cctx in
  Super_context.add_rule sctx ~dir
    (Action_builder.With_targets.add ~file_targets:[ output ]
       (let open Action_builder.With_targets.O in
       Action_builder.with_no_targets cm_deps
       >>> Command.run (Ok ctx.ocamlc) ~dir:(Path.build ctx.build_dir)
             ~stdout_to:output
             [ Command.Args.dyn ocaml_flags
             ; A "-I"
             ; Path (Path.build (Obj_dir.byte_dir obj_dir))
             ; Command.Args.as_any
                 (Lib_mode.Cm_kind.Map.get (CC.includes cctx) (Ocaml Cmo))
             ; opens modules m
             ; A "-short-paths"
             ; A "-i"
             ; Command.Ml_kind.flag Impl
             ; Dep src
             ]
       >>| Action.Full.add_sandbox sandbox))

module Alias_module = struct
  (* The alias module is an implementation detail to support wrapping library
     modules under a single toplevel name. Since OCaml doesn't have proper
     support for namespacing at the moment, in order to expose module `X` of
     library `foo` as `Foo.X`, Dune does the following:

     - it compiles x.ml to Foo__X.cmo, Foo__X.cmx, Foo__X.o, ... - it implicitly
     exposes a module alias [module X = Foo__X] to all the modules of the `foo`
     library

     The second point is achieved by implicitly opening a module containing such
     aliases for all modules of `foo` when compiling modules of `foo` via the
     `-open` option of the compiler. This module is called the alias module and
     is implicitly generated by Dune.*)

  type alias =
    { local_name : Module_name.t
    ; obj_name : Module_name.Unique.t
    }

  type t =
    { main_module : Module_name.t
    ; aliases : alias list
    ; shadowed : Module_name.t list
    }

  let to_ml { main_module; aliases; shadowed } =
    let b = Buffer.create 16 in
    Buffer.add_string b "(* generated by dune *)\n";
    let main_module = Module_name.to_string main_module in
    List.iter aliases ~f:(fun { local_name; obj_name } ->
        let local_name = Module_name.to_string local_name in
        Printf.bprintf b "\n(** @canonical %s.%s *)\nmodule %s = %s\n"
          main_module local_name local_name
          (Module_name.Unique.to_name ~loc:Loc.none obj_name
          |> Module_name.to_string));
    List.iter shadowed ~f:(fun shadowed ->
        Printf.bprintf b
          "\n\
           module %s = struct end\n\
           [@@deprecated \"this module is shadowed\"]\n"
          (Module_name.to_string shadowed));
    Buffer.contents b

  let of_modules project modules ~alias_module =
    let main_module = Modules.main_module_name modules |> Option.value_exn in
    let aliases =
      Modules.for_alias modules
      |> Module_name.Map.to_list_map ~f:(fun local_name m ->
             let obj_name = Module.obj_name m in
             { local_name; obj_name })
    in
    let shadowed =
      if Dune_project.dune_version project < (3, 5) then []
      else
        match Modules.lib_interface modules with
        | None -> []
        | Some m ->
          if Module.kind m = Alias then [] else [ Module.name alias_module ]
    in
    { main_module; aliases; shadowed }
end

let build_alias_module cctx alias_module =
  let modules = Compilation_context.modules cctx in
  let alias_file () =
    let project = Compilation_context.scope cctx |> Scope.project in
    Alias_module.of_modules project modules ~alias_module |> Alias_module.to_ml
  in
  let cctx = Compilation_context.for_alias_module cctx alias_module in
  let sctx = Compilation_context.super_context cctx in
  let file = Option.value_exn (Module.file alias_module ~ml_kind:Impl) in
  let dir = Compilation_context.dir cctx in
  let open Memo.O in
  let* () =
    Super_context.add_rule ~loc:Loc.none sctx ~dir
      (Action_builder.delayed alias_file
      |> Action_builder.write_file_dyn (Path.as_in_build_dir_exn file))
  in
  let cctx = Compilation_context.for_alias_module cctx alias_module in
  build_module cctx alias_module

let root_source entries =
  let b = Buffer.create 128 in
  List.iter entries ~f:(fun name ->
      Printf.bprintf b "module %s = %s\n"
        (Module_name.to_string name)
        (Module_name.to_string name));
  Buffer.contents b

let build_root_module cctx root_module =
  let entries = Compilation_context.root_module_entries cctx in
  let cctx = Compilation_context.for_root_module cctx root_module in
  let sctx = Compilation_context.super_context cctx in
  let file = Option.value_exn (Module.file root_module ~ml_kind:Impl) in
  let dir = Compilation_context.dir cctx in
  let open Memo.O in
  let* () =
    Super_context.add_rule ~loc:Loc.none sctx ~dir
      (let target = Path.as_in_build_dir_exn file in
       Action_builder.write_file_dyn target
         (let open Action_builder.O in
         let+ entries = entries in
         root_source entries))
  in
  build_module cctx root_module

let build_all cctx =
  let for_wrapped_compat = lazy (Compilation_context.for_wrapped_compat cctx) in
  let modules = Compilation_context.modules cctx in
  Memo.parallel_iter
    (Modules.fold_no_vlib modules ~init:[] ~f:(fun x acc -> x :: acc))
    ~f:(fun m ->
      match Module.kind m with
      | Root -> build_root_module cctx m
      | Alias -> build_alias_module cctx m
      | Wrapped_compat ->
        let cctx = Lazy.force for_wrapped_compat in
        build_module cctx m
      | _ ->
        let cctx =
          if Modules.is_stdlib_alias modules m then
            (* XXX it would probably be simpler if the flags were just for this
               module in the definition of the stanza *)
            Compilation_context.for_alias_module cctx m
          else cctx
        in
        build_module cctx m)

let with_empty_intf ~sctx ~dir module_ =
  let name =
    Module.file module_ ~ml_kind:Impl
    |> Option.value_exn
    |> Path.set_extension ~ext:".mli"
  in
  let rule =
    Action_builder.write_file
      (Path.as_in_build_dir_exn name)
      "(* Auto-generated by Dune *)"
  in
  let open Memo.O in
  let+ () = Super_context.add_rule sctx ~dir rule in
  Module.add_file module_ Ml_kind.Intf (Module.File.make Dialect.ocaml name)
