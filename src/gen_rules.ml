open Import
module Menhir_rules = Menhir
open Jbuild
open Build.O
open! No_io

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

module Gen(P : Install_rules.Params) = struct
  module Alias = Build_system.Alias
  module CC = Compilation_context
  module SC = Super_context
  module Odoc = Odoc.Gen(P)

  let sctx = P.sctx
  let ctx = SC.context sctx

  let stanzas_per_dir =
    List.map (SC.stanzas sctx) ~f:(fun stanzas ->
      (stanzas.SC.Dir_with_jbuild.ctx_dir, stanzas))
    |> Path.Map.of_list_exn

  (* +-----------------------------------------------------------------+
     | Interpretation of [modules] fields                              |
     +-----------------------------------------------------------------+ *)

  module Eval_modules = Ordered_set_lang.Make(Module.Name)(struct
      type t = (Module.t, Module.Name.t * Loc.t) result

      type key = Module.Name.t

      let key = function
        | Error (s, _) -> s
        | Ok m -> Module.name m
    end)

  let parse_modules ~(all_modules : Module.t Module.Name.Map.t)
        ~buildable:(conf : Buildable.t) =
    let fake_modules = ref Module.Name.Map.empty in
    let parse ~loc s =
      let name = Module.Name.of_string s in
      match Module.Name.Map.find all_modules name with
      | Some m -> Ok m
      | None ->
        fake_modules := Module.Name.Map.add !fake_modules name loc;
        Error (name, loc)
    in
    let modules =
      Eval_modules.eval_unordered
        conf.modules
        ~parse
        ~standard:(Module.Name.Map.map all_modules ~f:(fun m -> Ok m))
    in
    let only_present_modules modules =
      Module.Name.Map.filter_map ~f:(function
        | Ok m -> Some m
        | Error (s, loc) ->
          Loc.fail loc "Module %a doesn't exist." Module.Name.pp s
      ) modules
    in
    let modules = only_present_modules modules in
    let intf_only =
      Eval_modules.eval_unordered
        conf.modules_without_implementation
        ~parse
        ~standard:Module.Name.Map.empty
    in
    let intf_only = only_present_modules intf_only in
    Module.Name.Map.iteri !fake_modules ~f:(fun m loc ->
      Loc.warn loc "Module %a is excluded but it doesn't exist."
        Module.Name.pp m
    );
    let real_intf_only =
      Module.Name.Map.filter modules
        ~f:(fun (m : Module.t) -> Option.is_none m.impl)
    in
    if Module.Name.Map.equal intf_only real_intf_only
         ~equal:(fun a b -> Module.name a = Module.name b) then
      modules
    else begin
      let should_be_listed, shouldn't_be_listed =
        Module.Name.Map.merge intf_only real_intf_only ~f:(fun name x y ->
          match x, y with
          | Some _, Some _ -> None
          | None  , Some _ ->
            Some (Left  (String.uncapitalize (Module.Name.to_string name)))
          | Some _, None   ->
            Some (Right (String.uncapitalize (Module.Name.to_string name)))
          | None  , None   -> assert false)
        |> Module.Name.Map.values
        |> List.partition_map ~f:(fun x -> x)
      in
      let list_modules l =
        String.concat ~sep:"\n" (List.map l ~f:(sprintf "- %s"))
      in
      if should_be_listed <> [] then begin
        match Ordered_set_lang.loc conf.modules_without_implementation with
        | None ->
          Loc.warn conf.loc
            "Some modules don't have an implementation.\
             \nYou need to add the following field to this stanza:\
             \n\
             \n  %s\
             \n\
             \nThis will become an error in the future."
            (let tag = Sexp.unsafe_atom_of_string
                         "modules_without_implementation" in
             Sexp.to_string ~syntax:Dune
               (List [ tag
                     ; Sexp.To_sexp.(list string) should_be_listed
                     ]))
        | Some loc ->
          Loc.warn loc
            "The following modules must be listed here as they don't \
             have an implementation:\n\
             %s\n\
             This will become an error in the future."
            (list_modules should_be_listed)
      end;
      if shouldn't_be_listed <> [] then begin
        (* Re-evaluate conf.modules_without_implementation but this
           time keep locations *)
        let module Eval =
          Ordered_set_lang.Make(Module.Name)(struct
            type t = Loc.t * Module.t
            type key = Module.Name.t
            let key (_, m) = Module.name m
          end)
        in
        let parse ~loc s =
          let name = Module.Name.of_string s in
          match Module.Name.Map.find all_modules name with
          | Some m -> m
          | None -> Loc.fail loc "Module %s doesn't exist." s
        in
        let parse ~loc s = (loc, parse ~loc s) in
        let shouldn't_be_listed =
          Eval.eval_unordered conf.modules_without_implementation
            ~parse
            ~standard:(Module.Name.Map.map all_modules ~f:(fun m -> (Loc.none, m)))
          |> Module.Name.Map.values
          |> List.filter ~f:(fun (_, (m : Module.t)) ->
            Option.is_some m.impl)
        in
        (* CR-soon jdimino for jdimino: report all errors *)
        let loc, m = List.hd shouldn't_be_listed in
        Loc.fail loc
          "Module %a has an implementation, it cannot be listed here"
          Module.Name.pp m.name
      end;
      modules
    end

  let parse_mlds ~dir ~(all_mlds : string String.Map.t) ~mlds_written_by_user =
    if Ordered_set_lang.is_standard mlds_written_by_user then
      all_mlds
    else
      let mlds =
        Ordered_set_lang.String.eval_unordered
          mlds_written_by_user
          ~parse:(fun ~loc s ->
            match String.Map.find all_mlds s with
            | Some s ->
              s
            | None ->
              Loc.fail loc "%s.mld doesn't exist in %s" s
                (Path.to_string_maybe_quoted
                   (Path.drop_optional_build_context dir))
          )
          ~standard:all_mlds in
      mlds

  (* +-----------------------------------------------------------------+
     | User rules & copy files                                         |
     +-----------------------------------------------------------------+ *)

  let interpret_locks ~dir ~scope locks =
    List.map locks ~f:(SC.expand_vars_path sctx ~dir ~scope)

  let user_rule (rule : Rule.t) ~dir ~scope =
    let targets : SC.Action.targets =
      match rule.targets with
      | Infer -> Infer
      | Static fns -> Static (List.map fns ~f:(Path.relative dir))
    in
    SC.add_rule_get_targets sctx ~mode:rule.mode ~loc:rule.loc
      ~locks:(interpret_locks ~dir ~scope rule.locks)
      (SC.Deps.interpret_named sctx ~scope ~dir rule.deps
       >>>
       SC.Action.run
         sctx
         (snd rule.action)
         ~loc:(fst rule.action)
         ~dir
         ~bindings:(Pform.Map.of_bindings rule.deps)
         ~dep_kind:Required
         ~targets
         ~scope)

  let copy_files_rules (def: Copy_files.t) ~src_dir ~dir ~scope =
    let loc = String_with_vars.loc def.glob in
    let glob_in_src =
      let src_glob = SC.expand_vars_string sctx ~dir def.glob ~scope in
      Path.relative src_dir src_glob ~error_loc:loc
    in
    (* The following condition is required for merlin to work.
       Additionally, the order in which the rules are evaluated only
       ensures that [sources_and_targets_known_so_far] returns the
       right answer for sub-directories only. *)
    if not (Path.is_descendant glob_in_src ~of_:src_dir) then
      Loc.fail loc "%s is not a sub-directory of %s"
        (Path.to_string_maybe_quoted glob_in_src) (Path.to_string_maybe_quoted src_dir);
    let glob = Path.basename glob_in_src in
    let src_in_src = Path.parent_exn glob_in_src in
    let re =
      match Glob_lexer.parse_string glob with
      | Ok re ->
        Re.compile re
      | Error (_pos, msg) ->
        Loc.fail (String_with_vars.loc def.glob) "invalid glob: %s" msg
    in
    (* add rules *)
    let src_in_build = Path.append ctx.build_dir src_in_src in
    let files = SC.eval_glob sctx ~dir:src_in_build re in
    List.map files ~f:(fun basename ->
      let file_src = Path.relative src_in_build basename in
      let file_dst = Path.relative dir basename in
      SC.add_rule sctx
        ((if def.add_line_directive
          then Build.copy_and_add_line_directive
          else Build.copy)
           ~src:file_src
           ~dst:file_dst);
      file_dst)

  (* +-----------------------------------------------------------------+
     | "text" file listing                                             |
     +-----------------------------------------------------------------+ *)

  (* Compute the list of "text" files (.ml, .c, ...). This is the list
     of source files + user generated ones. As a side-effect, setup
     user rules and copy_files rules. *)
  let text_files =
    let cache = Hashtbl.create 32 in
    fun ~dir ->
      Hashtbl.find_or_add cache dir ~f:(fun dir ->
        match Path.Map.find stanzas_per_dir dir with
        | None -> String.Set.empty
        | Some { stanzas; src_dir; scope; _ } ->
          (* Interpret a few stanzas in order to determine the list of
             files generated by the user. *)
          let generated_files =
            List.concat_map stanzas ~f:(fun stanza ->
              match (stanza : Stanza.t) with
              | Menhir.T menhir ->
                Menhir_rules.targets menhir
              | Rule rule ->
                List.map (user_rule rule  ~dir ~scope) ~f:Path.basename
              | Copy_files def ->
                List.map (copy_files_rules def ~src_dir ~dir ~scope)
                  ~f:Path.basename
              | Library { buildable; _ } | Executables { buildable; _ } ->
                (* Manually add files generated by the (select ...)
                   dependencies *)
                List.filter_map buildable.libraries ~f:(fun dep ->
                  match (dep : Jbuild.Lib_dep.t) with
                  | Direct _ -> None
                  | Select s -> Some s.result_fn)
              | _ -> [])
            |> String.Set.of_list
          in
          String.Set.union generated_files
            (SC.source_files sctx ~src_path:src_dir))

  (* +-----------------------------------------------------------------+
     | Modules listing                                                 |
     +-----------------------------------------------------------------+ *)

  let guess_modules ~dir ~files =
    let impl_files, intf_files =
      String.Set.to_list files
      |> List.filter_partition_map ~f:(fun fn ->
        (* we aren't using Filename.extension because we want to handle
           filenames such as foo.cppo.ml *)
        match String.lsplit2 fn ~on:'.' with
        | Some (_, "ml") -> Left { Module.File.syntax=OCaml ; name=fn }
        | Some (_, "re") -> Left { Module.File.syntax=Reason ; name=fn }
        | Some (_, "mli") -> Right { Module.File.syntax=OCaml ; name=fn }
        | Some (_, "rei") -> Right { Module.File.syntax=Reason ; name=fn }
        | _ -> Skip)
    in
    let parse_one_set files =
      List.map files ~f:(fun (f : Module.File.t) ->
        (Module.Name.of_string (Filename.chop_extension f.name), f))
      |> Module.Name.Map.of_list
      |> function
      | Ok x -> x
      | Error (name, f1, f2) ->
        let src_dir = Path.drop_build_context_exn dir in
        die "too many files for module %a in %s: %s and %s"
          Module.Name.pp name (Path.to_string src_dir)
          f1.name f2.name
    in
    let impls = parse_one_set impl_files in
    let intfs = parse_one_set intf_files in
    Module.Name.Map.merge impls intfs ~f:(fun name impl intf ->
      Some (Module.make name ?impl ?intf))

  let guess_mlds ~files =
    String.Set.to_list files
    |> List.filter_map ~f:(fun fn ->
      match String.lsplit2 fn ~on:'.' with
      | Some (s, "mld") -> Some (s, fn)
      | _ -> None)
    |> String.Map.of_list_exn

  let mlds_by_dir =
    let cache = Hashtbl.create 32 in
    fun ~dir ->
      Hashtbl.find_or_add cache dir ~f:(fun dir ->
        let files = text_files ~dir in
        guess_mlds ~files)

  let mlds_of_dir (doc : Documentation.t) ~dir =
    parse_mlds ~dir
      ~all_mlds:(mlds_by_dir ~dir)
      ~mlds_written_by_user:doc.mld_files
    |> String.Map.values
    |> List.map ~f:(Path.relative dir)

  let modules_by_dir =
    let cache = Hashtbl.create 32 in
    fun ~dir ->
      Hashtbl.find_or_add cache dir ~f:(fun dir ->
        let files = text_files ~dir in
        guess_modules ~dir ~files)

  type modules_by_lib =
    { modules          : Module.t Module.Name.Map.t
    ; alias_module     : Module.t option
    ; main_module_name : Module.Name.t
    }

  let modules_by_lib =
    let cache = Hashtbl.create 32 in
    fun (lib : Library.t) ~dir ->
      Hashtbl.find_or_add cache (dir, lib.name) ~f:(fun _ ->
        let all_modules = modules_by_dir ~dir in
        let modules =
          parse_modules ~all_modules ~buildable:lib.buildable
        in
        let main_module_name = Module.Name.of_string lib.name in
        let modules =
          if not lib.wrapped then
            modules
          else
            Module.Name.Map.map modules ~f:(fun m ->
              if m.name = main_module_name then
                m
              else
                Module.with_wrapper m ~libname:lib.name)
        in
        let alias_module =
          if not lib.wrapped ||
             (Module.Name.Map.cardinal modules = 1 &&
              Module.Name.Map.mem modules main_module_name) then
            None
          else if Module.Name.Map.mem modules main_module_name then
            (* This module needs an implementation for non-jbuilder
               users of the library:

               https://github.com/ocaml/dune/issues/567 *)
            Some
              (Module.make (Module.Name.add_suffix main_module_name "__")
                 ~impl:{ name   = sprintf "%s__.ml-gen" lib.name
                       ; syntax = OCaml
                       }
                 ~obj_name:(lib.name ^ "__"))
          else
            Some
              (Module.make main_module_name
                 ~impl:{ name   = lib.name ^ ".ml-gen"
                       ; syntax = OCaml
                       }
                 ~obj_name:lib.name)
        in
        { modules; alias_module; main_module_name })

  let module_names_of_lib lib ~dir =
    let { modules; alias_module; _ } = modules_by_lib lib ~dir in
    let modules =
      match alias_module with
      | None -> modules
      | Some m -> Module.Name.Map.add modules m.name m
    in
    Module.Name.Map.values modules

  (* +-----------------------------------------------------------------+
     | Library stuff                                                   |
     +-----------------------------------------------------------------+ *)

  include Install_rules.Archives(P)

  let msvc_hack_cclibs cclibs =
    let f lib =
      if String.is_prefix lib ~prefix:"-l" then
        String.sub lib ~pos:2 ~len:(String.length lib - 2) ^ ".lib"
      else
        lib
    in
    let cclibs = List.map cclibs ~f in
    let f lib =
      if String.is_prefix lib ~prefix:"-l" then
        String.sub lib ~pos:2 ~len:(String.length lib - 2)
      else
        lib
    in
    List.map cclibs ~f

  let build_lib (lib : Library.t) ~scope ~flags ~dir ~obj_dir ~mode
        ~top_sorted_modules ~modules =
    Option.iter (Context.compiler ctx mode) ~f:(fun compiler ->
      let target = lib_archive lib ~dir ~ext:(Mode.compiled_lib_ext mode) in
      let stubs_flags =
        if not (Library.has_stubs lib) then
          []
        else
          let stubs_name = lib.name ^ "_stubs" in
          match mode with
          | Byte -> ["-dllib"; "-l" ^ stubs_name; "-cclib"; "-l" ^ stubs_name]
          | Native -> ["-cclib"; "-l" ^ stubs_name]
      in
      let map_cclibs =
        (* https://github.com/ocaml/dune/issues/119 *)
        if ctx.ccomp_type = "msvc" then
          msvc_hack_cclibs
        else
          fun x -> x
      in
      let artifacts ~ext modules =
        List.map modules ~f:(Module.obj_file ~obj_dir ~ext)
      in
      let obj_deps =
        Build.paths (artifacts modules ~ext:(Cm_kind.ext (Mode.cm_kind mode)))
      in
      let obj_deps =
        match mode with
        | Byte   -> obj_deps
        | Native ->
          obj_deps >>>
          Build.paths (artifacts modules ~ext:ctx.ext_obj)
      in
      SC.add_rule sctx
        (obj_deps
         >>>
         Build.fanout4
           (top_sorted_modules >>^artifacts ~ext:(Cm_kind.ext (Mode.cm_kind mode)))
           (SC.expand_and_eval_set sctx ~scope ~dir lib.c_library_flags
              ~standard:(Build.return []))
           (Ocaml_flags.get flags mode)
           (SC.expand_and_eval_set sctx ~scope ~dir lib.library_flags
              ~standard:(Build.return []))
         >>>
         Build.run ~context:ctx (Ok compiler)
           [ Dyn (fun (_, _, flags, _) -> As flags)
           ; A "-a"; A "-o"; Target target
           ; As stubs_flags
           ; Dyn (fun (_, cclibs, _, _) -> Arg_spec.quote_args "-cclib" (map_cclibs cclibs))
           ; Dyn (fun (_, _, _, library_flags) -> As library_flags)
           ; As (match lib.kind with
               | Normal -> []
               | Ppx_deriver | Ppx_rewriter -> ["-linkall"])
           ; Dyn (fun (cm_files, _, _, _) -> Deps cm_files)
           ; Hidden_targets
               (match mode with
                | Byte -> []
                | Native -> [lib_archive lib ~dir ~ext:ctx.ext_lib])
           ]))

  let build_c_file (lib : Library.t) ~scope ~dir ~includes c_name =
    let src = Path.relative dir (c_name ^ ".c") in
    let dst = Path.relative dir (c_name ^ ctx.ext_obj) in
    SC.add_rule sctx
      (SC.expand_and_eval_set sctx ~scope ~dir lib.c_flags
         ~standard:(Build.return (Context.cc_g ctx))
       >>>
       Build.run ~context:ctx
         (* We have to execute the rule in the library directory as
            the .o is produced in the current directory *)
         ~dir
         (Ok ctx.ocamlc)
         [ As (Utils.g ())
         ; includes
         ; Dyn (fun c_flags -> Arg_spec.quote_args "-ccopt" c_flags)
         ; A "-o"; Target dst
         ; Dep src
         ]);
    dst

  let build_cxx_file (lib : Library.t) ~scope ~dir ~includes c_name =
    let src = Path.relative dir (c_name ^ ".cpp") in
    let dst = Path.relative dir (c_name ^ ctx.ext_obj) in
    let open Arg_spec in
    let output_param =
      if ctx.ccomp_type = "msvc" then
        [Concat ("", [A "/Fo"; Target dst])]
      else
        [A "-o"; Target dst]
    in
    SC.add_rule sctx
      (SC.expand_and_eval_set sctx ~scope ~dir lib.cxx_flags
         ~standard:(Build.return (Context.cc_g ctx))
       >>>
       Build.run ~context:ctx
         (* We have to execute the rule in the library directory as
            the .o is produced in the current directory *)
         ~dir
         (SC.resolve_program sctx ctx.c_compiler)
         ([ S [A "-I"; Path ctx.stdlib_dir]
          ; As (SC.cxx_flags sctx)
          ; includes
          ; Dyn (fun cxx_flags -> As cxx_flags)
          ] @ output_param @
          [ A "-c"; Dep src
          ]));
    dst

  (* In 4.02, the compiler reads the cmi for module alias even with
     [-w -49 -no-alias-deps], so we must sandbox the build of the
     alias module since the modules it references are built after. *)
  let alias_module_build_sandbox = ctx.version < (4, 03, 0)

  let library_rules (lib : Library.t) ~modules_partitioner ~dir ~files ~scope
        ~compile_info ~dir_kind =
    let obj_dir = Utils.library_object_directory ~dir lib.name in
    let requires = Lib.Compile.requires compile_info in
    let dep_kind = if lib.optional then Build.Optional else Required in
    let flags = SC.ocaml_flags sctx ~scope ~dir lib.buildable in
    let { modules; main_module_name; alias_module } = modules_by_lib ~dir lib in
    let source_modules = modules in
    (* Preprocess before adding the alias module as it doesn't need
       preprocessing *)
    let pp =
      Preprocessing.make sctx ~dir ~dep_kind ~scope
        ~preprocess:lib.buildable.preprocess
        ~preprocessor_deps:
          (SC.Deps.interpret sctx ~scope ~dir
             lib.buildable.preprocessor_deps)
        ~lint:lib.buildable.lint
        ~lib_name:(Some lib.name)
        ~dir_kind
    in
    let modules = Preprocessing.pp_modules pp modules in

    let modules =
      match alias_module with
      | None -> modules
      | Some m -> Module.Name.Map.add modules m.name m
    in

    let lib_interface_module =
      if lib.wrapped then
        Module.Name.Map.find modules main_module_name
      else
        None
    in
    let cctx =
      Compilation_context.create ()
        ~super_context:sctx
        ~scope
        ~dir
        ~dir_kind
        ~obj_dir
        ~modules
        ?alias_module
        ?lib_interface_module
        ~flags
        ~requires
        ~preprocessing:pp
        ~no_keep_locs:lib.no_keep_locs
    in

    Modules_partitioner.acknowledge modules_partitioner cctx
      ~loc:lib.buildable.loc ~modules:source_modules;
    let dep_graphs = Ocamldep.rules cctx in

    Option.iter alias_module ~f:(fun m ->
      let file =
        match m.impl with
        | Some f -> f
        | None -> Option.value_exn m.intf
      in
      SC.add_rule sctx
        (Build.return
           (Module.Name.Map.values (Module.Name.Map.remove modules m.name)
            |> List.map ~f:(fun (m : Module.t) ->
              sprintf "(** @canonical %s.%s *)\n\
                       module %s = %s\n"
                (Module.Name.to_string main_module_name)
                (Module.Name.to_string m.name)
                (Module.Name.to_string m.name)
                (Module.Name.to_string (Module.real_unit_name m))
            )
            |> String.concat ~sep:"\n")
         >>> Build.write_file_dyn (Path.relative dir file.name)));


    let dynlink = lib.dynlink in
    let js_of_ocaml = lib.buildable.js_of_ocaml in
    Module_compilation.build_modules cctx ~js_of_ocaml ~dynlink ~dep_graphs;
    Option.iter alias_module ~f:(fun m ->
      let cctx = Compilation_context.for_alias_module cctx in
      Module_compilation.build_module cctx m
        ~js_of_ocaml
        ~dynlink
        ~sandbox:alias_module_build_sandbox
        ~dep_graphs:(Ocamldep.Dep_graphs.dummy m));

    if Library.has_stubs lib then begin
      let h_files =
        String.Set.to_list files
        |> List.filter_map ~f:(fun fn ->
          if String.is_suffix fn ~suffix:".h" then
            Some (Path.relative dir fn)
          else
            None)
      in
      let o_files =
        let includes =
          Arg_spec.S
            [ Hidden_deps h_files
            ; Arg_spec.of_result_map requires ~f:(fun libs ->
                S [ Lib.L.c_include_flags libs ~stdlib_dir:ctx.stdlib_dir
                  ; Hidden_deps (SC.Libs.file_deps sctx libs ~ext:".h")
                  ])
            ]
        in
        List.map lib.c_names ~f:(
          build_c_file   lib ~scope ~dir ~includes
        ) @ List.map lib.cxx_names ~f:(
          build_cxx_file lib ~scope ~dir ~includes
        )
      in
      match lib.self_build_stubs_archive with
      | Some _ -> ()
      | None ->
        let ocamlmklib ~sandbox ~custom ~targets =
          SC.add_rule sctx ~sandbox
            (SC.expand_and_eval_set sctx ~scope ~dir
               lib.c_library_flags ~standard:(Build.return [])
             >>>
             Build.run ~context:ctx
               (Ok ctx.ocamlmklib)
               [ As (Utils.g ())
               ; if custom then A "-custom" else As []
               ; A "-o"
               ; Path (Path.relative dir (sprintf "%s_stubs" lib.name))
               ; Deps o_files
               ; Dyn (fun cclibs ->
                   (* https://github.com/ocaml/dune/issues/119 *)
                   if ctx.ccomp_type = "msvc" then
                     let cclibs = msvc_hack_cclibs cclibs in
                     Arg_spec.quote_args "-ldopt" cclibs
                   else
                     As cclibs
                 )
               ; Hidden_targets targets
               ])
        in
        let static = stubs_archive lib ~dir in
        let dynamic = dll lib ~dir in
        let modes =
          Mode_conf.Set.eval lib.modes
            ~has_native:(Option.is_some ctx.ocamlopt)
        in
        if modes.native &&
           modes.byte   &&
           lib.dynlink
        then begin
          (* If we build for both modes and support dynlink, use a
             single invocation to build both the static and dynamic
             libraries *)
          ocamlmklib ~sandbox:false ~custom:false ~targets:[static; dynamic]
        end else begin
          ocamlmklib ~sandbox:false ~custom:true ~targets:[static];
          (* We can't tell ocamlmklib to build only the dll, so we
             sandbox the action to avoid overriding the static archive *)
          ocamlmklib ~sandbox:true ~custom:false ~targets:[dynamic]
        end
    end;

    List.iter Cm_kind.all ~f:(fun cm_kind ->
      let files =
        Module.Name.Map.fold modules ~init:Path.Set.empty ~f:(fun m acc ->
          match Module.cm_file m ~obj_dir cm_kind with
          | None -> acc
          | Some fn -> Path.Set.add acc fn)
      in
      SC.Libs.setup_file_deps_alias sctx ~dir lib ~ext:(Cm_kind.ext cm_kind)
        files);
    SC.Libs.setup_file_deps_group_alias sctx ~dir lib ~exts:[".cmi"; ".cmx"];
    SC.Libs.setup_file_deps_alias sctx ~dir lib ~ext:".h"
      (List.map lib.install_c_headers ~f:(fun header ->
         Path.relative dir (header ^ ".h"))
       |> Path.Set.of_list);

    (let modules =
       Module.Name.Map.fold modules ~init:[] ~f:(fun m acc ->
         if Module.has_impl m then
           m :: acc
         else
           acc)
     in
     let top_sorted_modules =
       Ocamldep.Dep_graph.top_closed_implementations dep_graphs.impl modules
     in
     List.iter Mode.all ~f:(fun mode ->
       build_lib lib ~scope ~flags ~dir ~obj_dir ~mode ~top_sorted_modules
         ~modules));
    (* Build *.cma.js *)
    SC.add_rules sctx (
      let src = lib_archive lib ~dir ~ext:(Mode.compiled_lib_ext Mode.Byte) in
      let target = Path.extend_basename src ~suffix:".js" in
      Js_of_ocaml_rules.build_cm sctx ~scope ~dir
        ~js_of_ocaml:lib.buildable.js_of_ocaml ~src ~target);

    if ctx.natdynlink_supported then
      Option.iter ctx.ocamlopt ~f:(fun ocamlopt ->
        let src = lib_archive lib ~dir ~ext:(Mode.compiled_lib_ext Native) in
        let dst = lib_archive lib ~dir ~ext:".cmxs" in
        let build =
          Build.dyn_paths (Build.arr (fun () -> [lib_archive lib ~dir ~ext:ctx.ext_lib]))
          >>>
          Ocaml_flags.get flags Native
          >>>
          Build.run ~context:ctx
            (Ok ocamlopt)
            [ Dyn (fun flags -> As flags)
            ; A "-shared"; A "-linkall"
            ; A "-I"; Path dir
            ; A "-o"; Target dst
            ; Dep src
            ]
        in
        let build =
          if Library.has_stubs lib then
            Build.path (stubs_archive ~dir lib)
            >>>
            build
          else
            build
        in
        SC.add_rule sctx build
      );

    Odoc.setup_library_odoc_rules lib ~requires ~modules ~dep_graphs ~scope;

    let flags =
      match alias_module with
      | None -> Ocaml_flags.common flags
      | Some m ->
        Ocaml_flags.prepend_common ["-open"; Module.Name.to_string m.name] flags
        |> Ocaml_flags.common
    in

    Sub_system.gen_rules
      { super_context = sctx
      ; dir
      ; stanza = lib
      ; scope
      ; source_modules
      ; compile_info
      };

    Merlin.make ()
      ~requires:(Lib.Compile.requires compile_info)
      ~flags
      ~preprocess:(Buildable.single_preprocess lib.buildable)
      ~libname:lib.name
      ~objs_dirs:(Path.Set.singleton obj_dir)

  let library_rules (lib : Library.t) ~modules_partitioner ~dir ~files ~scope
        ~dir_kind : Merlin.t =
    let compile_info =
      Lib.DB.get_compile_info (Scope.libs scope) lib.name
        ~allow_overlaps:lib.buildable.allow_overlapping_dependencies
    in
    SC.Libs.gen_select_rules sctx compile_info ~dir;
    SC.Libs.with_lib_deps sctx compile_info ~dir
      ~f:(fun () ->
        library_rules lib ~modules_partitioner ~dir ~files ~scope ~compile_info
          ~dir_kind)

  (* +-----------------------------------------------------------------+
     | Executables stuff                                               |
     +-----------------------------------------------------------------+ *)

  let executables_rules ~dir ~all_modules ~dir_kind
        ~modules_partitioner ~scope ~compile_info
        (exes : Executables.t) =
    let requires = Lib.Compile.requires compile_info in
    let modules =
      parse_modules ~all_modules ~buildable:exes.buildable
    in

    let preprocessor_deps =
      SC.Deps.interpret sctx exes.buildable.preprocessor_deps
        ~scope ~dir
    in
    let pp =
      Preprocessing.make sctx ~dir ~dep_kind:Required
        ~scope
        ~preprocess:exes.buildable.preprocess
        ~preprocessor_deps
        ~lint:exes.buildable.lint
        ~lib_name:None
        ~dir_kind
    in
    let modules =
      Module.Name.Map.map modules ~f:(fun m ->
        Preprocessing.pp_module_as pp m.name m)
    in

    let programs =
      List.map exes.names ~f:(fun (loc, name) ->
        let mod_name = Module.Name.of_string name in
        match Module.Name.Map.find modules mod_name with
        | Some m ->
          if not (Module.has_impl m) then
            Loc.fail loc "Module %a has no implementation."
              Module.Name.pp mod_name
          else
            { Exe.Program.name; main_module_name = mod_name }
        | None -> Loc.fail loc "Module %a doesn't exist."
                    Module.Name.pp mod_name)
    in

    let linkages =
      let module L = Executables.Link_mode in
      let l =
        let has_native = Option.is_some ctx.ocamlopt in
        List.filter_map (L.Set.to_list exes.modes) ~f:(fun (mode : L.t) ->
          if not has_native && mode.mode = Native then
            None
          else
            Some (Exe.Linkage.of_user_config ctx mode))
      in
      (* If bytecode was requested but not native or best version,
         add custom linking *)
      if L.Set.mem exes.modes L.byte         &&
         not (L.Set.mem exes.modes L.native) &&
         not (L.Set.mem exes.modes L.exe) then
        Exe.Linkage.custom :: l
      else
        l
    in

    let flags = SC.ocaml_flags sctx ~scope ~dir exes.buildable in
    let link_deps =
      SC.Deps.interpret sctx ~scope ~dir exes.link_deps
    in
    let link_flags =
      link_deps >>^ ignore >>>
      SC.expand_and_eval_set sctx exes.link_flags
        ~scope
        ~dir
        ~standard:(Build.return [])
    in

    (* Use "eobjs" rather than "objs" to avoid a potential conflict
       with a library of the same name *)
    let obj_dir =
      Utils.executable_object_directory ~dir (List.hd programs).name
    in

    let cctx =
      Compilation_context.create ()
        ~super_context:sctx
        ~scope
        ~dir
        ~dir_kind
        ~obj_dir
        ~modules
        ~flags
        ~requires
        ~preprocessing:pp
    in
    Modules_partitioner.acknowledge modules_partitioner cctx
      ~loc:exes.buildable.loc ~modules;

    Exe.build_and_link_many cctx
      ~programs
      ~linkages
      ~link_flags
      ~js_of_ocaml:exes.buildable.js_of_ocaml;

    Merlin.make ()
      ~requires:(Lib.Compile.requires compile_info)
      ~flags:(Ocaml_flags.common flags)
      ~preprocess:(Buildable.single_preprocess exes.buildable)
      ~objs_dirs:(Path.Set.singleton obj_dir)

  let executables_rules ~dir ~all_modules
        ~modules_partitioner ~scope ~dir_kind
        (exes : Executables.t) : Merlin.t =
    let compile_info =
      Lib.DB.resolve_user_written_deps (Scope.libs scope)
        exes.buildable.libraries
        ~pps:(Jbuild.Preprocess_map.pps exes.buildable.preprocess)
        ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
    in
    SC.Libs.gen_select_rules sctx compile_info ~dir;
    SC.Libs.with_lib_deps sctx compile_info ~dir
      ~f:(fun () ->
        executables_rules exes ~dir ~all_modules
          ~modules_partitioner ~scope ~compile_info ~dir_kind)

  (* +-----------------------------------------------------------------+
     | Aliases                                                         |
     +-----------------------------------------------------------------+ *)

  let add_alias ~dir ~name ~stamp ?(locks=[]) build =
    let alias = Build_system.Alias.make name ~dir in
    SC.add_alias_action sctx alias ~locks ~stamp build

  let alias_rules (alias_conf : Alias_conf.t) ~dir ~scope =
    let stamp =
      let module S = Sexp.To_sexp in
      Sexp.List
        [ Sexp.unsafe_atom_of_string "user-alias"
        ; Jbuild.Bindings.sexp_of_t Jbuild.Dep_conf.sexp_of_t alias_conf.deps
        ; S.option Action.Unexpanded.sexp_of_t
            (Option.map alias_conf.action ~f:snd)
        ]
    in
    add_alias
      ~dir
      ~name:alias_conf.name
      ~stamp
      ~locks:(interpret_locks ~dir ~scope alias_conf.locks)
      (SC.Deps.interpret_named sctx ~scope ~dir alias_conf.deps
       >>>
       match alias_conf.action with
       | None -> Build.progn []
       | Some (loc, action) ->
         SC.Action.run
           sctx
           action
           ~loc
           ~dir
           ~dep_kind:Required
           ~bindings:(Pform.Map.of_bindings alias_conf.deps)
           ~targets:Alias
           ~scope)

  let tests_rules (t : Tests.t) ~dir ~scope ~all_modules ~modules_partitioner
        ~dir_kind ~src_dir =
    let test_kind (loc, name) =
      let sources = SC.source_files sctx ~src_path:src_dir in
      let expected_basename = name ^ ".expected" in
      if String.Set.mem sources expected_basename then
        `Expect
          { Action.Unexpanded.Diff.
            file1 = String_with_vars.make_text loc expected_basename
          ; file2 = String_with_vars.make_text loc (name ^ ".output")
          ; optional = false
          ; mode = Text
          }
      else
        `Regular
    in
    let regular_rule run_action alias loc =
      { alias with Alias_conf.action = Some (loc, run_action) }
    in
    let expect_rule run_action (diff : Action.Unexpanded.Diff.t) alias loc =
      let rule =
        { Rule.
          targets = Infer
        ; deps = Bindings.empty
        ; action =
            (loc, Action.Unexpanded.Redirect (Stdout, diff.file2, run_action))
        ; mode = Standard
        ; locks = t.locks
        ; loc
        } in
      let alias =
        { alias with
          Alias_conf.
          action = Some (loc, Diff diff)
        ; locks = t.locks
        } in
      (alias, rule)
    in
    List.iter t.exes.names ~f:(fun (loc, s) ->
      let run_action =
        Action.Unexpanded.Run
          (String_with_vars.make_text loc ("./" ^ s ^ ".exe"), []) in
      let base_alias =
        { Alias_conf.
          name = "runtest"
        ; locks = []
        ; package = t.package
        ; deps = t.deps
        ; action = None
        } in
      match test_kind (loc, s) with
      | `Regular ->
        alias_rules ~dir ~scope (regular_rule run_action base_alias loc)
      | `Expect diff ->
        let (alias, rule) =
          expect_rule run_action diff base_alias loc in
        alias_rules ~dir ~scope alias;
        ignore (user_rule ~dir ~scope rule : Path.t list));
    executables_rules t.exes ~dir ~all_modules ~scope ~dir_kind
      ~modules_partitioner

  (* +-----------------------------------------------------------------+
     | Stanza                                                          |
     +-----------------------------------------------------------------+ *)

  let gen_rules { SC.Dir_with_jbuild. src_dir; ctx_dir; stanzas; scope; kind } =
    (* This interprets "rule" and "copy_files" stanzas. *)
    let files = text_files ~dir:ctx_dir in
    let all_modules = modules_by_dir ~dir:ctx_dir in
    let modules_partitioner = Modules_partitioner.create ~dir_kind:kind in
    let merlins =
      List.filter_map stanzas ~f:(fun stanza ->
        let dir = ctx_dir in
        match (stanza : Stanza.t) with
        | Library lib ->
          Some (library_rules lib ~dir ~files ~scope ~modules_partitioner
                  ~dir_kind:kind)
        | Executables exes ->
          Some (executables_rules exes ~dir ~all_modules ~scope
                  ~modules_partitioner ~dir_kind:kind)
        | Alias alias ->
          alias_rules alias ~dir ~scope;
          None
        | Tests tests ->
          Some (tests_rules tests ~dir ~scope ~all_modules ~src_dir
                  ~modules_partitioner ~dir_kind:kind)
        | Copy_files { glob; _ } ->
          let src_dir =
            let loc = String_with_vars.loc glob in
            let src_glob = SC.expand_vars_string sctx ~dir glob ~scope in
            Path.parent_exn (Path.relative src_dir src_glob ~error_loc:loc)
          in
          Some
            (Merlin.make ()
               ~source_dirs:(Path.Set.singleton src_dir))
        | _ -> None)
    in
    Option.iter (Merlin.merge_all merlins) ~f:(fun m ->
      Merlin.add_rules sctx ~dir:ctx_dir ~scope ~dir_kind:kind
        (Merlin.add_source_dir m src_dir));
    Utop.setup sctx ~dir:ctx_dir ~scope ~libs:(
      List.filter_map stanzas ~f:(function
        | Library lib -> Some lib
        | _ -> None));
    List.iter stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Menhir.T m ->
        begin match
          List.find_map (Menhir_rules.module_names m)
            ~f:(Modules_partitioner.find modules_partitioner)
        with
        | None ->
          (* This happens often when passing a [-p ...] option that
             hides a library *)
          let targets =
            List.map (Menhir_rules.targets m) ~f:(Path.relative ctx_dir)
          in
          SC.add_rule sctx
            (Build.fail ~targets
               { fail = fun () ->
                   Loc.fail m.loc
                     "I can't determine what library/executable the files \
                      produced by this stanza are part of."
               })
        | Some cctx ->
          Menhir_rules.gen_rules cctx m
        end
      | _ -> ());
    Modules_partitioner.emit_errors modules_partitioner

  let gen_rules ~dir components : Build_system.extra_sub_directories_to_keep =
    (match components with
     | ".js"  :: rest -> Js_of_ocaml_rules.setup_separate_compilation_rules
                           sctx rest
     | "_doc" :: rest -> Odoc.gen_rules rest ~dir
     | ".ppx"  :: rest -> Preprocessing.gen_rules sctx rest
     | _ ->
       match Path.Map.find stanzas_per_dir dir with
       | Some x -> gen_rules x
       | None ->
         if components <> [] &&
            Option.is_none
              (File_tree.find_dir (SC.file_tree sctx)
                 (Path.drop_build_context_exn dir)) then
           SC.load_dir sctx ~dir:(Path.parent_exn dir));
    match components with
    | [] -> These (String.Set.of_list [".js"; "_doc"; ".ppx"])
    | [(".js"|"_doc"|".ppx")] -> All
    | _  -> These String.Set.empty

  let init () =
    let module Install_rules =
      Install_rules.Gen(struct
        include P
        let module_names_of_lib = module_names_of_lib
        let mlds_of_dir = mlds_of_dir
      end) in
    Install_rules.init ();
    Odoc.init ~modules_by_lib:(fun ~dir lib ->
      let m = modules_by_lib ~dir lib in
      match m.alias_module with
      | Some alias_mod ->
        [Option.value ~default:alias_mod
           (Module.Name.Map.find m.modules m.main_module_name)]
      | None -> Module.Name.Map.values m.modules
    ) ~mlds_of_dir
end

module type Gen = sig
  val gen_rules
    :  dir:Path.t
    -> string list
    -> Build_system.extra_sub_directories_to_keep
  val init : unit -> unit
  val sctx : Super_context.t
end

let gen ~contexts ~build_system
      ?(external_lib_deps_mode=false)
      ?only_packages conf =
  let open Fiber.O in
  let { Jbuild_load. file_tree; jbuilds; packages; projects } = conf in
  let packages =
    match only_packages with
    | None -> packages
    | Some pkgs ->
      Package.Name.Map.filter packages ~f:(fun { Package.name; _ } ->
        Package.Name.Set.mem pkgs name)
  in
  let sctxs = Hashtbl.create 4 in
  List.iter contexts ~f:(fun c ->
    Hashtbl.add sctxs c.Context.name (Fiber.Ivar.create ()));
  let make_sctx (context : Context.t) : _ Fiber.t =
    let host () =
      match context.for_host with
      | None -> Fiber.return None
      | Some h ->
        Fiber.Ivar.read (Option.value_exn (Hashtbl.find sctxs h.name))
        >>| fun x -> Some x
    in
    let stanzas () =
      Jbuild_load.Jbuilds.eval ~context jbuilds >>| fun stanzas ->
      match only_packages with
      | None -> stanzas
      | Some pkgs ->
        List.map stanzas ~f:(fun (dir_conf : Jbuild_load.Jbuild.t) ->
          let stanzas =
            List.filter dir_conf.stanzas ~f:(fun stanza ->
              match (stanza : Stanza.t) with
              | Library { public = Some { package; _ }; _ }
              | Alias { package = Some package ;  _ }
              | Install { package; _ }
              | Documentation { package; _ } ->
                Package.Name.Set.mem pkgs package.name
              | _ -> true)
          in
          { dir_conf with stanzas })
    in
    Fiber.fork_and_join host stanzas >>= fun (host, stanzas) ->
    let sctx =
      Super_context.create
        ?host
        ~build_system
        ~context
        ~projects
        ~file_tree
        ~packages
        ~external_lib_deps_mode
        ~stanzas
    in
    let module M = Gen(struct let sctx = sctx end) in
    Fiber.Ivar.fill (Option.value_exn (Hashtbl.find sctxs context.name)) sctx
    >>| fun () ->
    (context.name, (module M : Gen))
  in
  Fiber.parallel_map contexts ~f:make_sctx >>| fun l ->
  let map = String.Map.of_list_exn l in
  Build_system.set_rule_generators build_system
    (String_map.map map ~f:(fun (module M : Gen) -> M.gen_rules));
  String_map.iter map ~f:(fun (module M : Gen) -> M.init ());
  String_map.map map ~f:(fun (module M : Gen) -> M.sctx)
