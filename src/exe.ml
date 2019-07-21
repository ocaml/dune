open! Stdune
open Import
open Build.O

module CC = Compilation_context
module SC = Super_context

module Program = struct
  type t =
    { name             : string
    ; main_module_name : Module.Name.t
    ; loc              : Loc.t
    }
end

module Linkage = struct
  type 'mode t =
    { mode  : 'mode
    ; ext   : string
    ; flags : string list
    }

  let map t ~f =
    { t with mode = f t.mode }

  let byte =
    { mode  = Mode.Js.Mode Byte
    ; ext   = ".bc"
    ; flags = []
    }

  let native =
    { mode  = Mode.Js.Mode Native
    ; ext   = ".exe"
    ; flags = []
    }

  let custom =
    { mode  = Mode.Js.Mode Byte
    ; ext   = ".exe"
    ; flags = ["-custom"]
    }

  let native_or_custom (context : Context.t) =
    match context.ocamlopt with
    | None   -> custom
    | Some _ -> native

  let make ~mode ~ext ?(flags=[]) () =
    { mode
    ; ext
    ; flags
    }

  let  c_flags = ["-output-obj"]
  let  o_flags = ["-output-complete-obj"]
  let so_flags_windows = o_flags
  let so_flags_unix    = ["-output-complete-obj"; "-runtime-variant"; "_pic"]

  let of_user_config (ctx : Context.t) (m : Dune_file.Executables.Link_mode.t) =
    let wanted_mode : Mode.Js.t =
      match m.mode with
      | Js -> Js
      | Byte -> Mode Byte
      | Native -> Mode Native
      | Best   -> Mode Native
    in
    let real_mode : Mode.Js.t =
      match m.mode with
      | Js -> Js
      | Byte -> Mode Byte
      | Native -> Mode Native
      | Best   -> if Option.is_some ctx.ocamlopt then Mode Native else Mode Byte
    in
    let ext =
      match wanted_mode with
      | Js    -> ".bc.js"
      | Mode mode -> begin
          match mode, m.kind with
          | Byte   , C             -> ".bc.c"
          | Native , C             -> User_error.raise ~loc:m.loc
                                        [ Pp.text "C file generation only \
                                                   supports bytecode!" ]
          | Byte   , Exe           -> ".bc"
          | Native , Exe           -> ".exe"
          | Byte   , Object        -> ".bc"  ^ ctx.lib_config.ext_obj
          | Native , Object        -> ".exe" ^ ctx.lib_config.ext_obj
          | Byte   , Shared_object -> ".bc"  ^ ctx.lib_config.ext_dll
          | Native , Shared_object ->          ctx.lib_config.ext_dll
        end
    in
    let flags =
      match m.kind with
      | C -> c_flags
      | Exe ->
        begin
          match wanted_mode, real_mode with
          | Mode Native, Mode Byte -> ["-custom"]
          | _ -> []
        end
      | Object -> o_flags
      | Shared_object ->
        let so_flags =
          if String.equal ctx.os_type "Win32" then
            so_flags_windows
          else
            so_flags_unix
        in
        match real_mode with
        | Mode Native ->
          (* The compiler doesn't pass these flags in native mode. This
             looks like a bug in the compiler. *)
          List.concat_map ctx.native_c_libraries ~f:(fun flag ->
            ["-cclib"; flag])
          @ so_flags
        | Mode Byte ->
          so_flags
        | Js ->
          Code_error.raise "js mode/shared object binary kind combination is illegal"
            []
    in
    { ext
    ; mode = real_mode
    ; flags
    }
end

let exe_path_from_name cctx ~name ~(linkage : Mode.t Linkage.t) =
  Path.Build.relative (CC.dir cctx) (name ^ linkage.ext)

let link_exe
      ~loc
      ~name
      ~(linkage:Mode.t Linkage.t)
      ~cm_files
      ~link_time_code_gen
      ~promote
      ?(link_flags=Build.arr (fun _ -> []))
      cctx
  =
  let sctx     = CC.super_context cctx in
  let ctx      = SC.context       sctx in
  let dir      = CC.dir           cctx in
  let requires = CC.requires_link cctx in
  let mode = linkage.mode in
  let exe = exe_path_from_name cctx ~name ~linkage in
  let compiler = Option.value_exn (Context.compiler ctx mode) in
  let top_sorted_cms = Cm_files.top_sorted_cms cm_files ~mode:linkage.mode in
  SC.add_rule sctx ~loc ~dir
    ~mode:(match promote with
      | None -> Standard
      | Some p -> Promote p)
    (let ocaml_flags = Ocaml_flags.get (CC.flags cctx) mode in
     let prefix =
       let dune_version =
         let scope = CC.scope cctx in
         let project = Scope.project scope in
         Dune_project.dune_version project
       in
       Build.ignore (
         if dune_version >= (2, 0) then
           Cm_files.unsorted_objects_and_cms cm_files ~mode
           |> Build.paths
         else
           Cm_files.top_sorted_objects_and_cms cm_files ~mode
           |> Build.dyn_paths
       )
     in
     prefix
     >>>
     Build.S.seq (Build.of_result_map requires ~f:(fun libs ->
       Build.paths (Lib.L.archive_files libs ~mode)))
       (Command.run ~dir:(Path.build ctx.build_dir)
          (Ok compiler)
          [ Command.Args.dyn ocaml_flags
          ; A "-o"; Target exe
          ; As linkage.flags
          ; Command.Args.dyn link_flags
          ; Command.of_result_map link_time_code_gen
              ~f:(fun { Link_time_code_gen.to_link; force_linkall } ->
                S [ As (if force_linkall then ["-linkall"] else [])
                  ; Lib.Lib_and_module.L.link_flags to_link ~mode
                  ])
          ; Dyn (Build.S.map top_sorted_cms ~f:(fun x -> Command.Args.Deps x))
          ]));
  exe

let link_js ~src ~cm_files ~promote cctx =
  let sctx     = CC.super_context cctx in
  let expander = CC.expander cctx in
  let js_of_ocaml =
    CC.js_of_ocaml cctx
    |> Option.value ~default:Dune_file.Js_of_ocaml.default
  in
  let flags =
    (Expander.expand_and_eval_set expander
       js_of_ocaml.flags
       ~standard:(Build.return (Js_of_ocaml_rules.standard sctx))) in
  let top_sorted_cms = Cm_files.top_sorted_cms cm_files ~mode:Mode.Byte in
  Js_of_ocaml_rules.build_exe cctx ~js_of_ocaml ~src
    ~cm:top_sorted_cms ~flags:(Command.Args.dyn flags)
    ~promote

let build_and_link_many
      ~programs
      ~linkages
      ~promote
      ?link_flags
      cctx
  =
  let modules = CC.modules cctx in
  let dep_graphs = Dep_rules.rules cctx ~modules in
  Module_compilation.build_all cctx ~dep_graphs;
  let link_time_code_gen = Link_time_code_gen.handle_special_libs cctx in
  List.iter programs ~f:(fun { Program.name; main_module_name ; loc } ->
    let cm_files =
      let sctx    = CC.super_context cctx in
      let ctx     = SC.context sctx in
      let obj_dir = CC.obj_dir cctx in
      let top_sorted_modules =
        let main = Option.value_exn (Modules.find modules main_module_name) in
        Dep_graph.top_closed_implementations dep_graphs.impl
          [main]
      in
      Cm_files.make ~obj_dir ~modules ~top_sorted_modules
        ~ext_obj:ctx.lib_config.ext_obj
    in
    List.iter linkages ~f:(fun linkage ->
      let has_js = linkage.Linkage.mode = Mode.Js.Js in
      let linkage = Linkage.map ~f:Mode.Js.to_mode linkage in
      let exe =
        link_exe cctx
          ~loc
          ~name
          ~linkage
          ~cm_files
          ~link_time_code_gen
          ~promote
          ?link_flags
      in
      if has_js then
        link_js ~src:exe ~cm_files ~promote cctx
    ))

let build_and_link ~program =
  build_and_link_many ~programs:[program]

let exe_path cctx ~(program : Program.t) ~linkage =
  exe_path_from_name cctx ~name:program.name ~linkage
