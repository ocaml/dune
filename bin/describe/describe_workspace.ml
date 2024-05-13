open Import

module Options = struct
  (* Option flags for what to do while crawling the workspace *)
  type t =
    { with_deps : bool (* whether to compute direct dependencies between modules *)
    ; with_pps : bool
    (* whether to include the dependencies to ppx-rewriters (that are
       used at compile time) *)
    }

  (* whether to sanitize absolute paths of workspace items, and their UIDs, to
     ensure reproducible tests *)
  let sanitize_for_tests = ref false

  let arg_with_deps =
    let open Arg in
    value
    & flag
    & info
        [ "with-deps" ]
        ~doc:"Whether the dependencies between modules should be printed."
  ;;

  let arg_with_pps =
    let open Arg in
    value
    & flag
    & info
        [ "with-pps" ]
        ~doc:
          "Whether the dependencies towards ppx-rewriters (that are called at compile \
           time) should be taken into account."
  ;;

  let arg_sanitize_for_tests =
    let open Arg in
    value
    & flag
    & info
        [ "sanitize-for-tests" ]
        ~doc:
          "Sanitize the absolute paths in workspace items, and the associated UIDs, so \
           that the output is reproducible."
  ;;

  let arg : t Term.t =
    let+ with_deps = arg_with_deps
    and+ with_pps = arg_with_pps
    and+ sanitize_for_tests_value = arg_sanitize_for_tests in
    sanitize_for_tests := sanitize_for_tests_value;
    { with_deps; with_pps }
  ;;
end

(* The module [Descr] is a typed representation of the description of a
   workspace, that is provided by the ``dune describe workspace`` command.

   Each sub-module contains a [to_dyn] function, that translates the
   descriptors to a value of type [Dyn.t].

   The typed representation aims at precisely describing the structure of the
   information computed by ``dune describe``, and hopefully make users' life
   easier in decoding the S-expressions into meaningful contents. *)
module Descr = struct
  (* [dyn_path p] converts a path to a value of type [Dyn.t]. Remark: this is
     different from Path.to_dyn, that produces extra tags from a variant
     datatype. *)
  let dyn_path (p : Path.t) : Dyn.t = String (Path.to_string p)

  (* Description of the dependencies of a module *)
  module Mod_deps = struct
    type t =
      { for_intf : Dune_rules.Module_name.t list
          (* direct module dependencies for the interface *)
      ; for_impl : Dune_rules.Module_name.t list
      (* direct module dependencies for the implementation *)
      }

    (* Conversion to the [Dyn.t] type *)
    let to_dyn { for_intf; for_impl } =
      let open Dyn in
      record
        [ "for_intf", list Dune_rules.Module_name.to_dyn for_intf
        ; "for_impl", list Dune_rules.Module_name.to_dyn for_impl
        ]
    ;;
  end

  (* Description of modules *)
  module Mod = struct
    type t =
      { name : Dune_rules.Module_name.t (* name of the module *)
      ; impl : Path.t option (* path to the .ml file, if any *)
      ; intf : Path.t option (* path to the .mli file, if any *)
      ; cmt : Path.t option (* path to the .cmt file, if any *)
      ; cmti : Path.t option (* path to the .cmti file, if any *)
      ; module_deps : Mod_deps.t (* direct module dependencies *)
      }

    (* Conversion to the [Dyn.t] type *)
    let to_dyn { Options.with_deps; _ } { name; impl; intf; cmt; cmti; module_deps }
      : Dyn.t
      =
      let open Dyn in
      let optional_fields =
        let module_deps =
          if with_deps then Some ("module_deps", Mod_deps.to_dyn module_deps) else None
        in
        (* we build a list of options, that is later filtered, so that adding
           new optional fields in the future can be done easily *)
        match module_deps with
        | None -> []
        | Some module_deps -> [ module_deps ]
      in
      record
      @@ [ "name", Dune_rules.Module_name.to_dyn name
         ; "impl", option dyn_path impl
         ; "intf", option dyn_path intf
         ; "cmt", option dyn_path cmt
         ; "cmti", option dyn_path cmti
         ]
      @ optional_fields
    ;;
  end

  (* Description of executables *)
  module Exe = struct
    type t =
      { names : string list (* names of the executable *)
      ; requires : Digest.t list
          (* list of direct dependencies to libraries, identified by their
             digests *)
      ; modules : Mod.t list (* list of the modules the executable is composed of *)
      ; include_dirs : Path.t list (* list of include directories *)
      }

    let map_path t ~f = { t with include_dirs = List.map ~f t.include_dirs }

    (* Conversion to the [Dyn.t] type *)
    let to_dyn options { names; requires; modules; include_dirs } : Dyn.t =
      let open Dyn in
      record
        [ "names", List (List.map ~f:(fun name -> String name) names)
        ; "requires", Dyn.(list string) (List.map ~f:Digest.to_string requires)
        ; "modules", list (Mod.to_dyn options) modules
        ; "include_dirs", list dyn_path include_dirs
        ]
    ;;
  end

  (* Description of libraries *)

  module Lib = struct
    type t =
      { name : Lib_name.t (* name of the library *)
      ; uid : Digest.t (* digest of the library *)
      ; local : bool (* whether this library is local *)
      ; requires : Digest.t list
          (* list of direct dependendies to libraries, identified by their
             digests *)
      ; source_dir : Path.t
          (* path to the directory that contains the sources of this library *)
      ; modules : Mod.t list (* list of the modules the executable is composed of *)
      ; include_dirs : Path.t list (* list of include directories *)
      }

    let map_path t ~f =
      { t with source_dir = f t.source_dir; include_dirs = List.map ~f t.include_dirs }
    ;;

    (* Conversion to the [Dyn.t] type *)
    let to_dyn options { name; uid; local; requires; source_dir; modules; include_dirs }
      : Dyn.t
      =
      let open Dyn in
      record
        [ "name", Lib_name.to_dyn name
        ; "uid", String (Digest.to_string uid)
        ; "local", Bool local
        ; "requires", (list string) (List.map ~f:Digest.to_string requires)
        ; "source_dir", dyn_path source_dir
        ; "modules", list (Mod.to_dyn options) modules
        ; "include_dirs", (list dyn_path) include_dirs
        ]
    ;;
  end

  (* Description of items: executables, or libraries *)
  module Item = struct
    type t =
      | Executables of Exe.t
      | Library of Lib.t
      | Root of Path.t
      | Build_context of Path.t

    let map_path t ~f =
      match t with
      | Executables exe -> Executables (Exe.map_path exe ~f)
      | Library lib -> Library (Lib.map_path lib ~f)
      | Root r -> Root (f r)
      | Build_context c -> Build_context (f c)
    ;;

    (* Conversion to the [Dyn.t] type *)
    let to_dyn options : t -> Dyn.t = function
      | Executables exe_descr -> Variant ("executables", [ Exe.to_dyn options exe_descr ])
      | Library lib_descr -> Variant ("library", [ Lib.to_dyn options lib_descr ])
      | Root root -> Variant ("root", [ String (Path.to_absolute_filename root) ])
      | Build_context build_ctxt ->
        Variant ("build_context", [ String (Path.to_string build_ctxt) ])
    ;;
  end

  (* Description of a workspace: a list of items *)
  module Workspace = struct
    type t = Item.t list

    (* Conversion to the [Dyn.t] type *)
    let to_dyn options (items : t) : Dyn.t = Dyn.list (Item.to_dyn options) items
  end
end

module Lang = struct
  type t = Dune_lang.Syntax.Version.t

  let arg_conv =
    let parser s =
      match Scanf.sscanf s "%u.%u" (fun a b -> a, b) with
      | Ok t -> Ok t
      | Error () -> Error (`Msg "Expected version of the form NNN.NNN.")
    in
    let printer ppf t =
      Stdlib.Format.fprintf ppf "%s" (Dune_lang.Syntax.Version.to_string t)
    in
    Arg.conv ~docv:"VERSION" (parser, printer)
  ;;

  let arg : t Term.t =
    Term.ret
    @@ let+ v =
         Arg.(
           value
           & opt arg_conv (0, 1)
           & info
               [ "lang" ]
               ~docv:"VERSION"
               ~doc:"Behave the same as this version of Dune.")
       in
       if v = (0, 1)
       then `Ok v
       else (
         let msg =
           let pp =
             "Only --lang 0.1 is available at the moment as this command is not yet \
              stabilised. If you would like to release a software that relies on the \
              output of 'dune describe', please open a ticket on \
              https://github.com/ocaml/dune."
             |> Pp.text
           in
           Stdlib.Format.asprintf "%a" Pp.to_fmt pp
         in
         `Error (true, msg))
  ;;
end

(* The following module is responsible sanitizing the output of
   [dune describe workspace], so that the absolute paths and the UIDs that
   depend on them are stable for tests. These paths may differ, depending on
   the machine they are run on. *)
module Sanitize_for_tests = struct
  module Workspace = struct
    let fake_findlib = lazy (Path.External.of_string "/FINDLIB")
    let fake_workspace = lazy (Path.External.of_string "/WORKSPACE_ROOT")

    let sanitize_with_findlib ~findlib_paths path =
      let path = Path.external_ path in
      List.find_map findlib_paths ~f:(fun candidate ->
        let open Option.O in
        let* candidate = Path.as_external candidate in
        (* if the path to rename is an external path, try to find the
           OCaml root inside, and replace it with a fixed string *)
        let+ without_prefix = Path.drop_prefix ~prefix:(Path.external_ candidate) path in
        (* we have found the OCaml root path: let's replace it with a
           constant string *)
        Path.External.append_local (Lazy.force fake_findlib) without_prefix)
    ;;

    (* Sanitizes a workspace description, by renaming non-reproducible UIDs and
       paths *)
    let really_sanitize ~findlib_paths items =
      let rename_path = function
        (* we have found a path for OCaml's root: let's define the renaming
           function *)
        | Path.External path ->
          sanitize_with_findlib ~findlib_paths path
          |> Option.value ~default:path
          |> Path.external_
        | In_source_tree p ->
          (* Replace the workspace root with a fixed string *)
          Path.External.append_local (Lazy.force fake_workspace) (Path.Source.to_local p)
          |> Path.external_
        | path ->
          (* Otherwise, it should not be changed *)
          path
      in
      (* now, we rename the UIDs in the [requires] field , while reversing the
         list of items, so that we get back the original ordering *)
      List.map ~f:(Descr.Item.map_path ~f:rename_path) items
    ;;

    (* Sanitizes a workspace description when options ask to do so, or performs
       no change at all otherwise *)
    let sanitize ~findlib_paths items =
      if !Options.sanitize_for_tests then really_sanitize ~findlib_paths items else items
    ;;
  end
end

(* Crawl the workspace to get all the data *)
module Crawl = struct
  open Dune_rules
  open Dune_engine
  open Memo.O

  (* Computes the digest of a library *)
  let uid_of_library (lib : Lib.t) : Digest.t =
    let name = Lib.name lib in
    if Lib.is_local lib
    then (
      let source_dir = Lib_info.src_dir (Lib.info lib) in
      Digest.generic (name, Path.to_string source_dir))
    else Digest.generic name
  ;;

  let immediate_deps_of_module ~options ~obj_dir ~modules unit =
    match (options : Options.t) with
    | { with_deps = false; _ } ->
      Action_builder.return { Ocaml.Ml_kind.Dict.intf = []; impl = [] }
    | { with_deps = true; _ } ->
      let deps ml_kind =
        Dune_rules.Dep_rules.immediate_deps_of unit modules ~obj_dir ~ml_kind
      in
      let open Action_builder.O in
      let+ intf, impl = Action_builder.both (deps Intf) (deps Impl) in
      { Ocaml.Ml_kind.Dict.intf; impl }
  ;;

  (* Builds the description of a module from a module and its object directory *)
  let module_
    ~obj_dir
    ~(deps_for_intf : Module.t list)
    ~(deps_for_impl : Module.t list)
    (m : Module.t)
    : Descr.Mod.t
    =
    let source ml_kind = Option.map (Module.source m ~ml_kind) ~f:Module.File.path in
    let cmt ml_kind =
      Dune_rules.Obj_dir.Module.cmt_file obj_dir m ~ml_kind ~cm_kind:(Ocaml Cmi)
    in
    { Descr.Mod.name = Module.name m
    ; impl = source Impl
    ; intf = source Intf
    ; cmt = cmt Impl
    ; cmti = cmt Intf
    ; module_deps =
        { for_intf = List.map ~f:Module.name deps_for_intf
        ; for_impl = List.map ~f:Module.name deps_for_impl
        }
    }
  ;;

  (* Builds the list of modules *)
  let modules ~obj_dir ~deps_of modules_ : Descr.Mod.t list Memo.t =
    modules_
    |> Modules.With_vlib.drop_vlib
    |> Modules.fold ~init:(Memo.return []) ~f:(fun m macc ->
      let* acc = macc in
      let deps = deps_of m in
      let+ { Ocaml.Ml_kind.Dict.intf = deps_for_intf; impl = deps_for_impl }, _ =
        Dune_engine.Action_builder.evaluate_and_collect_facts deps
      in
      module_ ~obj_dir ~deps_for_intf ~deps_for_impl m :: acc)
  ;;

  (* Builds a workspace item for the provided executables object *)
  let executables sctx ~options ~project ~dir (exes : Executables.t)
    : (Descr.Item.t * Lib.Set.t) option Memo.t
    =
    let first_exe = snd (Nonempty_list.hd exes.names) in
    let* scope =
      Scope.DB.find_by_project (Super_context.context sctx |> Context.name) project
    in
    let* modules_, obj_dir =
      let+ modules_, obj_dir =
        Dir_contents.get sctx ~dir
        >>= Dir_contents.ocaml
        >>= Ml_sources.modules_and_obj_dir
              ~libs:(Scope.libs scope)
              ~for_:(Exe { first_exe })
      in
      Modules.With_vlib.modules modules_, obj_dir
    in
    let* pp_map =
      let+ version =
        let+ ocaml = Super_context.context sctx |> Context.ocaml in
        ocaml.version
      in
      Staged.unstage
      @@ Pp_spec.pped_modules_map
           (Preprocess.Per_module.without_instrumentation exes.buildable.preprocess)
           version
    in
    let deps_of module_ =
      let module_ = pp_map module_ in
      immediate_deps_of_module ~options ~obj_dir ~modules:modules_ module_
    in
    let obj_dir = Obj_dir.of_local obj_dir in
    let* modules_ = modules ~obj_dir ~deps_of modules_ in
    let+ requires =
      let* compile_info = Exe_rules.compile_info ~scope exes in
      let open Resolve.Memo.O in
      let* requires = Lib.Compile.direct_requires compile_info in
      if options.with_pps
      then
        let+ pps = Lib.Compile.pps compile_info in
        pps @ requires
      else Resolve.Memo.return requires
    in
    match Resolve.peek requires with
    | Error () -> None
    | Ok libs ->
      let include_dirs = Obj_dir.all_cmis obj_dir in
      let exe_descr =
        { Descr.Exe.names = List.map ~f:snd (Nonempty_list.to_list exes.names)
        ; requires = List.map ~f:uid_of_library libs
        ; modules = modules_
        ; include_dirs
        }
      in
      Some (Descr.Item.Executables exe_descr, Lib.Set.of_list libs)
  ;;

  (* Builds a workspace item for the provided library object *)
  let library sctx ~options (lib : Lib.t) : Descr.Item.t option Memo.t =
    let* requires = Lib.requires lib in
    match Resolve.peek requires with
    | Error () -> Memo.return None
    | Ok requires ->
      let name = Lib.name lib in
      let info = Lib.info lib in
      let src_dir = Lib_info.src_dir info in
      let obj_dir = Lib_info.obj_dir info in
      let+ modules_ =
        match Lib.is_local lib with
        | false -> Memo.return []
        | true ->
          (* XXX why do we have a second object directory? *)
          let* modules_, obj_dir_ =
            let* libs =
              Scope.DB.find_by_dir (Path.as_in_build_dir_exn src_dir) >>| Scope.libs
            in
            let+ modules_, obj_dir_ =
              Dir_contents.get sctx ~dir:(Path.as_in_build_dir_exn src_dir)
              >>= Dir_contents.ocaml
              >>= Ml_sources.modules_and_obj_dir
                    ~libs
                    ~for_:(Library (Lib_info.lib_id info |> Lib_id.to_local_exn))
            in
            Modules.With_vlib.modules modules_, obj_dir_
          in
          let* pp_map =
            let+ version =
              let+ ocaml = Super_context.context sctx |> Context.ocaml in
              ocaml.version
            in
            Staged.unstage
            @@ Pp_spec.pped_modules_map
                 (Preprocess.Per_module.without_instrumentation
                    (Lib_info.preprocess info))
                 version
          in
          let deps_of module_ =
            immediate_deps_of_module
              ~options
              ~obj_dir:obj_dir_
              ~modules:modules_
              (pp_map module_)
          in
          modules ~obj_dir ~deps_of modules_
      in
      let include_dirs = Obj_dir.all_cmis obj_dir in
      let lib_descr =
        { Descr.Lib.name
        ; uid = uid_of_library lib
        ; local = Lib.is_local lib
        ; requires = List.map requires ~f:uid_of_library
        ; source_dir = src_dir
        ; modules = modules_
        ; include_dirs
        }
      in
      Some (Descr.Item.Library lib_descr)
  ;;

  (* [source_path_is_in_dirs dirs p] tests whether the source path [p] is a
     descendant of some of the provided directory [dirs]. If [dirs = None],
     then it always succeeds. If [dirs = Some l], then a matching directory is
     search in the list [l]. *)
  let source_path_is_in_dirs dirs (p : Path.Source.t) =
    match dirs with
    | None -> true
    | Some dirs -> List.exists ~f:(fun dir -> Path.Source.is_descendant p ~of_:dir) dirs
  ;;

  (* Tests whether a dune file is located in a path that is a descendant of
     some directory *)
  let dune_file_is_in_dirs dirs dune_file =
    Dune_file.dir dune_file |> source_path_is_in_dirs dirs
  ;;

  (* Tests whether a library is located in a path that is a descendant of some
     directory *)
  let lib_is_in_dirs dirs (lib : Lib.t) =
    source_path_is_in_dirs
      dirs
      (Path.drop_build_context_exn @@ Lib_info.best_src_dir @@ Lib.info lib)
  ;;

  (* Builds a workspace item for the root path *)
  let root () = Descr.Item.Root Path.root

  (* Builds a workspace item for the build directory path *)
  let build_ctxt (context : Context.t) : Descr.Item.t =
    Descr.Item.Build_context (Path.build (Context.build_dir context))
  ;;

  (* Builds a workspace description for the provided dune setup and context *)
  let workspace
    options
    ({ Dune_rules.Main.contexts = _; scontexts } : Dune_rules.Main.build_system)
    (context : Context.t)
    dirs
    : Descr.Workspace.t Memo.t
    =
    let context_name = Context.name context in
    let sctx = Context_name.Map.find_exn scontexts context_name in
    let open Memo.O in
    let* dune_files =
      Dune_load.dune_files context_name >>| List.filter ~f:(dune_file_is_in_dirs dirs)
    in
    let* exes, exe_libs =
      (* the list of workspace items that describe executables, and the list of
         their direct library dependencies *)
      Memo.parallel_map dune_files ~f:(fun (dune_file : Dune_file.t) ->
        Dune_file.stanzas dune_file
        >>= Memo.parallel_map ~f:(fun stanza ->
          match Stanza.repr stanza with
          | Executables.T exes ->
            let dir =
              Path.Build.append_source
                (Context.build_dir context)
                (Dune_file.dir dune_file)
            in
            let project = Dune_file.project dune_file in
            executables sctx ~options ~project ~dir exes
          | _ -> Memo.return None)
        >>| List.filter_opt)
      >>| List.concat
      >>| List.split
    in
    let exe_libs =
      (* conflate the dependencies of executables into a single set *)
      Lib.Set.union_all exe_libs
    in
    let* project_libs =
      (* the list of libraries declared in the project *)
      Dune_load.projects ()
      >>= Memo.parallel_map ~f:(fun project ->
        Scope.DB.find_by_project (Context.name context) project
        >>| Scope.libs
        >>= Lib.DB.all)
      >>| Lib.Set.union_all
      >>| Lib.Set.filter ~f:(lib_is_in_dirs dirs)
    in
    let+ libs =
      (* the executables' libraries, and the project's libraries *)
      Lib.Set.union exe_libs project_libs
      |> Lib.Set.to_list
      |> Lib.descriptive_closure ~with_pps:options.with_pps
      >>= Memo.parallel_map ~f:(library ~options sctx)
      >>| List.filter_opt
    in
    let root = root () in
    let build_ctxt = build_ctxt context in
    root :: build_ctxt :: (exes @ libs)
  ;;
end

let find_dir common dir =
  let p = Path.Source.(relative root) (Common.prefix_target common dir) in
  let s = Path.source p in
  if not @@ Path.exists s
  then User_error.raise [ Pp.textf "No such file or directory: %s" (Path.to_string s) ];
  if not @@ Path.is_directory s
  then
    User_error.raise
      [ Pp.textf "File exists, but is not a directory: %s" (Path.to_string s) ];
  Memo.return p
;;

let term : unit Term.t =
  let+ builder = Common.Builder.term
  and+ what =
    Arg.(
      value
      & pos_all string []
      & info
          []
          ~docv:"DIRS"
          ~doc:
            "prints a description of the workspace's structure. If some directories DIRS \
             are provided, then only those directories of the workspace are considered.")
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ format = Describe_format.arg
  and+ lang = Lang.arg
  and+ options = Options.arg in
  let common, config = Common.init builder in
  let dirs =
    let args = "workspace" :: what in
    let parse =
      Dune_lang.Syntax.set Stanza.syntax (Active lang)
      @@
      let open Dune_lang.Decoder in
      fields
      @@ field "workspace"
      @@ let+ dirs = repeat relative_file in
         (* [None] means that all directories should be accepted,
            whereas [Some l] means that only the directories in the
            list [l] should be accepted. The checks on whether the
            paths exist and whether they are directories are performed
            later in the [describe] function. *)
         let dirs = if List.is_empty dirs then None else Some dirs in
         dirs
    in
    let ast =
      Dune_lang.Ast.add_loc
        ~loc:Loc.none
        (List (List.map args ~f:Dune_lang.atom_or_quoted_string))
    in
    Dune_lang.Decoder.parse parse Univ_map.empty ast
  in
  Scheduler.go ~common ~config
  @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  Build_system.run_exn
  @@ fun () ->
  let open Memo.O in
  let* setup = setup in
  let super_context = Import.Main.find_scontext_exn setup ~name:context_name in
  let context = Super_context.context super_context in
  let* findlib_paths = Context.findlib_paths context in
  (* prefix directories with the workspace root, so that the
     command also works correctly when it is run from a
     subdirectory *)
  Memo.Option.map dirs ~f:(Memo.List.map ~f:(find_dir common))
  >>= Crawl.workspace options setup context
  >>| Sanitize_for_tests.Workspace.sanitize ~findlib_paths
  >>| Descr.Workspace.to_dyn options
  >>| Describe_format.print_dyn format
;;

let command =
  let doc =
    "Print a description of the workspace's structure. If some directories DIRS are \
     provided, then only those directories of the workspace are considered."
  in
  let info = Cmd.info ~doc "workspace" in
  Cmd.v info term
;;
