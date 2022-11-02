open Import
module CC = Compilation_context

module Program = struct
  type t =
    { name : string
    ; main_module_name : Module_name.t
    ; loc : Loc.t
    }
end

module Linkage = struct
  type t =
    { mode : Link_mode.t
    ; ext : string
    ; flags : string list
    }

  let byte = { mode = Byte; ext = ".bc"; flags = [] }

  let byte_for_jsoo =
    { mode = Byte_for_jsoo
    ; ext = ".bc-for-jsoo"
    ; flags = [ "-no-check-prims" ]
    }

  let native = { mode = Native; ext = ".exe"; flags = [] }

  let is_native x = x.mode = Native

  let is_js x = x.mode = Byte && x.ext = ".bc.js"

  let is_byte x = x.mode = Byte && not (is_js x)

  let custom_with_ext ~ext context =
    { mode = Byte_with_stubs_statically_linked_in
    ; ext
    ; flags =
        [ Ocaml.Version.custom_or_output_complete_exe context.Context.version ]
    }

  let custom = custom_with_ext ~ext:".exe"

  let native_or_custom (context : Context.t) =
    match context.ocamlopt with
    | Error _ -> custom context
    | Ok _ -> native

  let js = { mode = Byte; ext = ".bc.js"; flags = [] }

  let is_plugin t =
    List.mem (List.map ~f:Mode.plugin_ext Mode.all) t.ext ~equal:String.equal

  let c_flags = [ "-output-obj" ]

  let o_flags = [ "-output-complete-obj" ]

  let so_flags_windows = o_flags

  let so_flags_unix = [ "-output-complete-obj"; "-runtime-variant"; "_pic" ]

  let cmxs_flags = [ "-shared" ]

  let cma_flags = [ "-a" ]

  let of_user_config (ctx : Context.t) ~loc
      (m : Dune_file.Executables.Link_mode.t) =
    match m with
    | Other { mode = Byte; kind = Js } -> js
    | _ ->
      let link_mode : Link_mode.t =
        match m with
        | Byte_complete -> Byte_with_stubs_statically_linked_in
        | Other { mode; _ } -> (
          match mode with
          | Byte ->
            if ctx.dynamically_linked_foreign_archives then Byte
            else
              (* When [dynamically_linked_foreign_archives] is set to [false] in
                 the workspace, we link in all stub archives statically into the
                 runtime system. *)
              Byte_with_stubs_statically_linked_in
          | Native -> Native
          | Best ->
            if Result.is_ok ctx.ocamlopt then Native
            else Byte_with_stubs_statically_linked_in)
      in
      let ext =
        Dune_file.Executables.Link_mode.extension m ~loc
          ~ext_obj:ctx.lib_config.ext_obj ~ext_dll:ctx.lib_config.ext_dll
      in
      let flags =
        match m with
        | Byte_complete ->
          [ Ocaml.Version.custom_or_output_complete_exe ctx.version ]
        | Other { kind; _ } -> (
          match kind with
          | C -> c_flags
          | Js -> []
          | Exe -> (
            match link_mode with
            | Byte_with_stubs_statically_linked_in ->
              [ Ocaml.Version.custom_or_output_complete_exe ctx.version ]
            | _ -> [])
          | Object -> o_flags
          | Plugin -> (
            match link_mode with
            | Native -> cmxs_flags
            | _ -> cma_flags)
          | Shared_object -> (
            let so_flags =
              let os_type = Ocaml_config.os_type ctx.ocaml_config in
              if os_type = Win32 then so_flags_windows else so_flags_unix
            in
            match link_mode with
            | Native ->
              (* The compiler doesn't pass these flags in native mode. This
                 looks like a bug in the compiler. *)
              let native_c_libraries =
                Ocaml_config.native_c_libraries ctx.ocaml_config
              in
              List.concat_map native_c_libraries ~f:(fun flag ->
                  [ "-cclib"; flag ])
              @ so_flags
            | Byte | Byte_for_jsoo | Byte_with_stubs_statically_linked_in ->
              so_flags))
      in
      { ext; mode = link_mode; flags }
end

let exe_path_from_name cctx ~name ~(linkage : Linkage.t) =
  Path.Build.relative (CC.dir cctx) (name ^ linkage.ext)

let link_exe ~loc ~name ~(linkage : Linkage.t) ~cm_files ~link_time_code_gen
    ~promote ~link_args ~o_files ?(sandbox = Sandbox_config.default) cctx =
  let sctx = CC.super_context cctx in
  let ctx = Super_context.context sctx in
  let dir = CC.dir cctx in
  let mode = Link_mode.mode linkage.mode in
  let exe = exe_path_from_name cctx ~name ~linkage in
  let top_sorted_cms = Cm_files.top_sorted_cms cm_files ~mode in
  let fdo_linker_script = Fdo.Linker_script.create cctx (Path.build exe) in
  let open Memo.O in
  let* action_with_targets =
    let ocaml_flags = Ocaml_flags.get (CC.flags cctx) mode in
    let prefix =
      Cm_files.top_sorted_objects_and_cms cm_files ~mode
      |> Action_builder.dyn_paths_unit
    in
    let+ fdo_linker_script_flags = Fdo.Linker_script.flags fdo_linker_script in
    let open Action_builder.With_targets.O in
    (* NB. Below we take care to pass [link_args] last on the command-line for
       the following reason: [link_args] contains the list of foreign libraries
       being linked into the executable; on some systems (eg Linux), the linker
       discards symbols of libraries passed on the command-line if those symbols
       have not yet been referenced when the library is processed (even if they
       are referenced by objects coming later on the command-line). In
       particular, this can cause the link operation to fail if some of the
       symbols in the foreign libraries are referenced only from foreign stubs
       (which are in [o_files]) if [o_files] appears after [link_args].

       While this fix works, more principled solutions should be explored:

       - Having foreign stubs declare dependencies on foreign libraries
       explicitly in the dune file.

       - Implicitly declaring a dependency of all foreign stubs on all foreign
       libraries.

       In each case, we could then pass the argument in dependency order, which
       would provide a better fix for this issue. *)
    Action_builder.with_no_targets prefix
    >>> Command.run ~dir:(Path.build ctx.build_dir)
          (Context.compiler ctx mode)
          [ Command.Args.dyn ocaml_flags
          ; A "-o"
          ; Target exe
          ; As linkage.flags
          ; Resolve.args
              (let open Resolve.O in
              let+ { Link_time_code_gen.to_link; force_linkall } =
                link_time_code_gen
              in
              Command.Args.S
                [ As (if force_linkall then [ "-linkall" ] else [])
                ; Lib_flags.Lib_and_module.L.link_flags sctx to_link
                    ~lib_config:ctx.lib_config ~mode:linkage.mode
                ])
          ; Deps o_files
          ; Dyn
              (Action_builder.map top_sorted_cms ~f:(fun x ->
                   Command.Args.Deps x))
          ; fdo_linker_script_flags
          ; Dyn link_args
          ]
    >>| Action.Full.add_sandbox sandbox
  in
  Super_context.add_rule sctx ~loc ~dir
    ~mode:
      (match promote with
      | None -> Standard
      | Some p -> Promote p)
    action_with_targets

let link_js ~name ~loc ~cm_files ~promote ~link_time_code_gen cctx =
  let in_context =
    CC.js_of_ocaml cctx |> Option.value ~default:Js_of_ocaml.In_context.default
  in
  let other_cm =
    let open Memo.O in
    let+ { Link_time_code_gen.to_link; force_linkall = _ } =
      Resolve.read_memo link_time_code_gen
    in
    List.map to_link ~f:(function
      | Lib_flags.Lib_and_module.Lib lib -> `Lib lib
      | Module (obj_dir, m) ->
        let path =
          Obj_dir.Module.cm_file_exn obj_dir m ~kind:(Ocaml (Mode.cm_kind Byte))
        in
        `Mod path)
  in
  let src = exe_path_from_name cctx ~name ~linkage:Linkage.byte_for_jsoo in
  let top_sorted_cms = Cm_files.top_sorted_cms cm_files ~mode:Mode.Byte in
  Jsoo_rules.build_exe cctx ~loc ~in_context ~src ~cm:top_sorted_cms ~promote
    ~link_time_code_gen:other_cm

type dep_graphs = { for_exes : Module.t list Action_builder.t list }

let link_many ?(link_args = Action_builder.return Command.Args.empty) ?o_files
    ?(embed_in_plugin_libraries = []) ?sandbox ~programs ~linkages ~promote cctx
    =
  let open Memo.O in
  let o_files =
    match o_files with
    | None -> Mode.Map.empty
    | Some o_files -> o_files
  in
  let modules = Compilation_context.modules cctx in
  let* link_time_code_gen = Link_time_code_gen.handle_special_libs cctx in
  let+ for_exes =
    Memo.parallel_map programs
      ~f:(fun { Program.name; main_module_name; loc } ->
        let top_sorted_modules =
          let main =
            match Modules.find modules main_module_name with
            | Some m -> m
            | None ->
              Code_error.raise "link_many: unable to find module"
                [ ("main_module_name", Module_name.to_dyn main_module_name)
                ; ("modules", Modules.to_dyn modules)
                ]
          in
          Dep_graph.top_closed_implementations (CC.dep_graphs cctx).impl
            [ main ]
        in
        let cm_files =
          let sctx = CC.super_context cctx in
          let ctx = Super_context.context sctx in
          let obj_dir = CC.obj_dir cctx in
          Cm_files.make ~obj_dir ~modules ~top_sorted_modules
            ~ext_obj:ctx.lib_config.ext_obj ()
        in
        let+ () =
          Memo.parallel_iter linkages ~f:(fun linkage ->
              if Linkage.is_js linkage then
                link_js ~loc ~name ~cm_files ~promote cctx ~link_time_code_gen
              else
                let* link_time_code_gen =
                  match Linkage.is_plugin linkage with
                  | false -> Memo.return link_time_code_gen
                  | true ->
                    let cc =
                      CC.for_plugin_executable cctx ~embed_in_plugin_libraries
                    in
                    Link_time_code_gen.handle_special_libs cc
                in
                let link_args, o_files =
                  let select_o_files =
                    Mode.Map.Multi.for_only ~and_all:true o_files
                  in
                  match linkage.mode with
                  | Native -> (link_args, select_o_files Mode.Native)
                  | Byte | Byte_for_jsoo | Byte_with_stubs_statically_linked_in
                    -> (link_args, select_o_files Mode.Byte)
                in
                link_exe cctx ~loc ~name ~linkage ~cm_files ~link_time_code_gen
                  ~promote ~link_args ~o_files ?sandbox)
        in
        top_sorted_modules)
  in
  { for_exes }

let build_and_link_many ?link_args ?o_files ?embed_in_plugin_libraries ?sandbox
    ~programs ~linkages ~promote cctx =
  let open Memo.O in
  let* () = Module_compilation.build_all cctx in
  link_many ?link_args ?o_files ?embed_in_plugin_libraries ?sandbox ~programs
    ~linkages ~promote cctx

let build_and_link ?link_args ?o_files ?embed_in_plugin_libraries ?sandbox
    ~program ~linkages ~promote cctx =
  build_and_link_many ?link_args ?o_files ?embed_in_plugin_libraries ?sandbox
    ~programs:[ program ] ~linkages ~promote cctx

let exe_path cctx ~(program : Program.t) ~linkage =
  exe_path_from_name cctx ~name:program.name ~linkage
