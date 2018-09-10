open! Stdune
open Import
module Menhir_rules = Menhir
open Dune_file
open Build.O
open! No_io

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

module Gen(P : Install_rules.Params) = struct
  module Alias = Build_system.Alias
  module CC = Compilation_context
  module SC = Super_context
  module Lib_rules = Lib_rules.Gen(P)

  let sctx = P.sctx
  let ctx = SC.context sctx

  let opaque = SC.opaque sctx

  (* +-----------------------------------------------------------------+
     | Executables stuff                                               |
     +-----------------------------------------------------------------+ *)

  let executables_rules ~dir ~dir_kind
        ~dir_contents ~scope ~compile_info
        (exes : Executables.t) =
    (* Use "eobjs" rather than "objs" to avoid a potential conflict
       with a library of the same name *)
    let obj_dir =
      Utils.executable_object_directory ~dir (List.hd exes.names |> snd)
    in
    let requires = Lib.Compile.requires compile_info in
    let modules =
      Dir_contents.modules_of_executables dir_contents
        ~first_exe:(snd (List.hd exes.names))
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
            Errors.fail loc "Module %a has no implementation."
              Module.Name.pp mod_name
          else
            { Exe.Program.name; main_module_name = mod_name }
        | None -> Errors.fail loc "Module %a doesn't exist."
                    Module.Name.pp mod_name)
    in

    let linkages =
      let module L = Executables.Link_mode in
      let l =
        let has_native = Option.is_some ctx.ocamlopt in
        List.filter_map (L.Set.to_list exes.modes) ~f:(fun (mode : L.t) ->
          match has_native, mode.mode with
          | false, Native ->
            None
          | _ ->
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
        ~opaque
    in

    Exe.build_and_link_many cctx
      ~programs
      ~linkages
      ~link_flags
      ~js_of_ocaml:exes.buildable.js_of_ocaml;

    (cctx,
     Merlin.make ()
       ~requires:(Lib.Compile.requires compile_info)
       ~flags:(Ocaml_flags.common flags)
       ~preprocess:(Buildable.single_preprocess exes.buildable)
       ~objs_dirs:(Path.Set.singleton obj_dir))

  let executables_rules ~dir
        ~dir_contents ~scope ~dir_kind
        (exes : Executables.t) : Compilation_context.t * Merlin.t =
    let compile_info =
      Lib.DB.resolve_user_written_deps (Scope.libs scope)
        exes.buildable.libraries
        ~pps:(Dune_file.Preprocess_map.pps exes.buildable.preprocess)
        ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
    in
    SC.Libs.gen_select_rules sctx compile_info ~dir;
    SC.Libs.with_lib_deps sctx compile_info ~dir
      ~f:(fun () ->
        executables_rules exes ~dir
          ~dir_contents ~scope ~compile_info ~dir_kind)

  (* +-----------------------------------------------------------------+
     | Tests                                                           |
     +-----------------------------------------------------------------+ *)

  let tests_rules (t : Tests.t) ~dir ~scope ~dir_contents ~dir_kind =
    let test_kind (loc, name) =
      let files = Dir_contents.text_files dir_contents in
      let expected_basename = name ^ ".expected" in
      if String.Set.mem files expected_basename then
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
    List.iter t.exes.names ~f:(fun (loc, s) ->
      let test_var_name = "test" in
      let run_action =
        match t.action with
        | Some a -> a
        | None -> Action.Unexpanded.Run (String_with_vars.make_var loc test_var_name, [])
      in
      let test_exe = s ^ ".exe" in
      let test_exe_path = Super_context.Action.map_exe sctx (Path.relative dir test_exe) in
      let extra_bindings = Pform.Map.singleton test_var_name (Values [Path test_exe_path]) in
      let add_alias ~loc ~action ~locks =
        let alias =
          { Alias_conf.
            name = "runtest"
          ; locks
          ; package = t.package
          ; deps = t.deps
          ; action = Some (loc, action)
          ; enabled_if = t.enabled_if
          ; loc
          }
        in
        Simple_rules.alias sctx ~extra_bindings ~dir ~scope alias
      in
      match test_kind (loc, s) with
      | `Regular ->
          add_alias ~loc ~action:run_action ~locks:[]
      | `Expect diff ->
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
        add_alias ~loc ~action:(Diff diff) ~locks:t.locks;
        ignore (Simple_rules.user_rule sctx rule ~extra_bindings ~dir ~scope : Path.t list));
    executables_rules t.exes ~dir ~scope ~dir_kind
      ~dir_contents

  (* +-----------------------------------------------------------------+
     | Stanza                                                          |
     +-----------------------------------------------------------------+ *)

  let gen_rules dir_contents
        { SC.Dir_with_jbuild. src_dir; ctx_dir; stanzas; scope; kind } =
    let merlins, cctxs =
      let rec loop stanzas merlins cctxs =
        let dir = ctx_dir in
        match stanzas with
        | [] -> (List.rev merlins, List.rev cctxs)
        | stanza :: stanzas ->
          match (stanza : Stanza.t) with
          | Library lib ->
            let cctx, merlin =
              Lib_rules.rules lib ~dir ~scope ~dir_contents ~dir_kind:kind
            in
            loop stanzas (merlin :: merlins)
              ((lib.buildable.loc, cctx) :: cctxs)
          | Executables exes ->
            let cctx, merlin =
              executables_rules exes ~dir ~scope
                ~dir_contents ~dir_kind:kind
            in
            loop stanzas (merlin :: merlins)
              ((exes.buildable.loc, cctx) :: cctxs)
          | Alias alias ->
            Simple_rules.alias sctx alias ~dir ~scope;
            loop stanzas merlins cctxs
          | Tests tests ->
            let cctx, merlin =
              tests_rules tests ~dir ~scope ~dir_contents ~dir_kind:kind
            in
            loop stanzas (merlin :: merlins)
              ((tests.exes.buildable.loc, cctx) :: cctxs)
          | Copy_files { glob; _ } ->
            let src_dir =
              let loc = String_with_vars.loc glob in
              let src_glob = SC.expand_vars_string sctx ~dir glob ~scope in
              Path.parent_exn (Path.relative src_dir src_glob ~error_loc:loc)
            in
            let merlin =
              Merlin.make ()
                ~source_dirs:(Path.Set.singleton src_dir)
            in
            loop stanzas (merlin :: merlins) cctxs
          | _ ->
            loop stanzas merlins cctxs
      in
      loop stanzas [] []
    in
    Option.iter (Merlin.merge_all merlins) ~f:(fun m ->
      let more_src_dirs =
        List.map (Dir_contents.dirs dir_contents) ~f:(fun dc ->
          Path.drop_optional_build_context (Dir_contents.dir dc))
      in
      Merlin.add_rules sctx ~dir:ctx_dir ~more_src_dirs ~scope ~dir_kind:kind
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
            ~f:(fun name ->
              Option.bind (Dir_contents.lookup_module dir_contents name)
                ~f:(fun buildable ->
                  List.find_map cctxs ~f:(fun (loc, cctx) ->
                    Option.some_if (Loc.equal loc buildable.loc) cctx)))
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
                   Errors.fail m.loc
                     "I can't determine what library/executable the files \
                      produced by this stanza are part of."
               })
        | Some cctx ->
          Menhir_rules.gen_rules cctx m
        end
      | _ -> ())

  let gen_rules dir_contents ~dir =
    match SC.stanzas_in sctx ~dir with
    | None -> ()
    | Some d -> gen_rules dir_contents d

  let gen_rules ~dir components : Build_system.extra_sub_directories_to_keep =
    (match components with
     | ".js"  :: rest -> Js_of_ocaml_rules.setup_separate_compilation_rules
                           sctx rest
     | "_doc" :: rest -> Lib_rules.Odoc.gen_rules rest ~dir
     | ".ppx"  :: rest -> Preprocessing.gen_rules sctx rest
     | _ ->
       match
         File_tree.find_dir (SC.file_tree sctx)
           (Path.drop_build_context_exn dir)
       with
       | None ->
         (* We get here when [dir] is a generated directory, such as
            [.utop] or [.foo.objs]. *)
         if components <> [] then SC.load_dir sctx ~dir:(Path.parent_exn dir)
       | Some _ ->
         (* This interprets "rule" and "copy_files" stanzas. *)
         let dir_contents = Dir_contents.get sctx ~dir in
         match Dir_contents.kind dir_contents with
         | Standalone ->
           gen_rules dir_contents ~dir
         | Group_part root ->
           SC.load_dir sctx ~dir:(Dir_contents.dir root)
         | Group_root (lazy subs) ->
           gen_rules dir_contents ~dir;
           List.iter subs ~f:(fun dc ->
             gen_rules dir_contents ~dir:(Dir_contents.dir dc)));
    match components with
    | [] -> These (String.Set.of_list [".js"; "_doc"; ".ppx"])
    | [(".js"|"_doc"|".ppx")] -> All
    | _  -> These String.Set.empty

  let init () =
    let module Install_rules =
      Install_rules.Gen(P)
    in
    Install_rules.init ();
    Lib_rules.Odoc.init ()
end

module type Gen = sig
  val gen_rules
    :  dir:Path.t
    -> string list
    -> Build_system.extra_sub_directories_to_keep
  val init : unit -> unit
  val sctx : Super_context.t
end

let stanza_package = function
  | Library { public = Some { package; _ }; _ }
  | Alias { package = Some package ;  _ }
  | Install { package; _ }
  | Documentation { package; _ }
  | Tests { package = Some package; _} ->
      Some package
  | _ -> None

let relevant_stanzas pkgs stanzas =
  List.filter stanzas ~f:(fun stanza ->
    match stanza_package stanza with
    | Some package -> Package.Name.Set.mem pkgs package.name
    | None -> true)

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
          { dir_conf with
            stanzas = relevant_stanzas pkgs dir_conf.stanzas
          })
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
    (String.Map.map map ~f:(fun (module M : Gen) -> M.gen_rules));
  String.Map.iter map ~f:(fun (module M : Gen) -> M.init ());
  String.Map.map map ~f:(fun (module M : Gen) -> M.sctx)
