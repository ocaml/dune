open Import
open Stdune

module Options = struct
  type t = Describe_common.Descr.options

  (** whether to sanitize absolute paths of workspace items, and their UIDs, to
      ensure reproducible tests *)
  let sanitize_for_tests = ref false

  let arg_with_deps =
    let open Arg in
    value & flag
    & info [ "with-deps" ]
        ~doc:"Whether the dependencies between modules should be printed."

  let arg_with_pps =
    let open Arg in
    value & flag
    & info [ "with-pps" ]
        ~doc:
          "Whether the dependencies towards ppx-rewriters (that are called at \
           compile time) should be taken into account."

  let arg_sanitize_for_tests =
    let open Arg in
    value & flag
    & info [ "sanitize-for-tests" ]
        ~doc:
          "Sanitize the absolute paths in workspace items, and the associated \
           UIDs, so that the output is reproducible."

  let arg : t Term.t =
    let+ with_deps = arg_with_deps
    and+ with_pps = arg_with_pps
    and+ sanitize_for_tests_value = arg_sanitize_for_tests in
    sanitize_for_tests := sanitize_for_tests_value;
    { Describe_common.Descr.with_deps; with_pps }
end

module Lang = struct
  type t = Dune_lang.Syntax.Version.t

  let arg_conv =
    let parser s =
      match Scanf.sscanf s "%u.%u" (fun a b -> (a, b)) with
      | Ok t -> Ok t
      | Error () -> Error (`Msg "Expected version of the form NNN.NNN.")
    in
    let printer ppf t =
      Stdlib.Format.fprintf ppf "%s" (Dune_lang.Syntax.Version.to_string t)
    in
    Arg.conv ~docv:"VERSION" (parser, printer)

  let arg : t Term.t =
    Term.ret
    @@ let+ v =
         Arg.(
           value
           & opt arg_conv (0, 1)
           & info [ "lang" ] ~docv:"VERSION"
               ~doc:"Behave the same as this version of Dune.")
       in
       if v = (0, 1) then `Ok v
       else
         let msg =
           let pp =
             "Only --lang 0.1 is available at the moment as this command is \
              not yet stabilised. If you would like to release a software that \
              relies on the output of 'dune describe', please open a ticket on \
              https://github.com/ocaml/dune." |> Pp.text
           in
           Stdlib.Format.asprintf "%a" Pp.to_fmt pp
         in
         `Error (true, msg)
end

(** The following module is responsible sanitizing the output of
    [dune describe workspace], so that the absolute paths and the UIDs that
    depend on them are stable for tests. These paths may differ, depending on
    the machine they are run on. *)
module Sanitize_for_tests = struct
  module Workspace = struct
    (** Sanitizes a workspace description, by renaming non-reproducible UIDs and
        paths *)
    let really_sanitize (context : Context.t) items =
      let rename_path =
        let findlib_paths =
          context.findlib_paths |> List.map ~f:Path.to_string
        in
        function
        (* we have found a path for OCaml's root: let's define the renaming
           function *)
        | Path.External ext_path as path -> (
          match
            List.find_map findlib_paths ~f:(fun prefix ->
                (* if the path to rename is an external path, try to find the
                   OCaml root inside, and replace it with a fixed string *)
                let s = Path.External.to_string ext_path in
                match String.drop_prefix ~prefix s with
                | None -> None
                | Some s' ->
                  (* we have found the OCaml root path: let's replace it with a
                     constant string *)
                  Some
                    (Path.external_
                    @@ Path.External.of_string
                         Filename.(concat dir_sep @@ concat "FINDLIB" s')))
          with
          | None -> path
          | Some p -> p)
        | Path.In_source_tree p ->
          (* Replace the workspace root with a fixed string *)
          let p =
            let new_root = Filename.(concat dir_sep "WORKSPACE_ROOT") in
            if Path.Source.is_root p then new_root
            else Filename.(concat new_root (Path.Source.to_string p))
          in
          Path.external_ (Path.External.of_string p)
        | path ->
          (* Otherwise, it should not be changed *)
          path
      in
      (* now, we rename the UIDs in the [requires] field , while reversing the
         list of items, so that we get back the original ordering *)
      List.map ~f:(Describe_common.Descr.Item.map_path ~f:rename_path) items

    (** Sanitizes a workspace description when options ask to do so, or performs
        no change at all otherwise *)
    let sanitize context items =
      if !Options.sanitize_for_tests then really_sanitize context items
      else items
  end
end

(** Crawl the workspace to get all the data *)
module Crawl = struct
  open Describe_common
  open Dune_rules
  open Dune_engine
  open Memo.O

  (** Computes the digest of a library *)
  let uid_of_library (lib : Lib.t) : Digest.t =
    let name = Lib.name lib in
    if Lib.is_local lib then
      let source_dir = Lib_info.src_dir (Lib.info lib) in
      Digest.generic (name, Path.to_string source_dir)
    else Digest.generic name

  let immediate_deps_of_module ~options ~obj_dir ~modules unit =
    match (options : Describe_common.Descr.options) with
    | { with_deps = false; _ } ->
      Action_builder.return { Ocaml.Ml_kind.Dict.intf = []; impl = [] }
    | { with_deps = true; _ } ->
      let deps = Dune_rules.Dep_rules.immediate_deps_of unit modules obj_dir in
      let open Action_builder.O in
      let+ intf, impl = Action_builder.both (deps Intf) (deps Impl) in
      { Ocaml.Ml_kind.Dict.intf; impl }

  (** Builds the description of a module from a module and its object directory *)
  let module_ ~obj_dir ~(deps_for_intf : Module.t list)
      ~(deps_for_impl : Module.t list) (m : Module.t) : Descr.Mod.t =
    let source ml_kind =
      Option.map (Module.source m ~ml_kind) ~f:Module.File.path
    in
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

  (** Builds the list of modules *)
  let modules ~obj_dir ~deps_of modules_ : Descr.Mod.t list Memo.t =
    Modules.fold_no_vlib ~init:(Memo.return []) modules_ ~f:(fun m macc ->
        let* acc = macc in
        let deps = deps_of m in
        let+ ( { Ocaml.Ml_kind.Dict.intf = deps_for_intf; impl = deps_for_impl }
             , _ ) =
          Dune_engine.Action_builder.run deps Eager
        in
        module_ ~obj_dir ~deps_for_intf ~deps_for_impl m :: acc)

  (** Builds a workspace item for the provided executables object *)
  let executables sctx ~options ~project ~dir (exes : Dune_file.Executables.t) :
      (Descr.Item.t * Lib.Set.t) option Memo.t =
    let first_exe = snd (List.hd exes.names) in
    let* modules_, obj_dir =
      Dir_contents.get sctx ~dir >>= Dir_contents.ocaml
      >>| Ml_sources.modules_and_obj_dir ~for_:(Exe { first_exe })
    in
    let pp_map =
      Staged.unstage
      @@
      let version = (Super_context.context sctx).ocaml.version in
      Preprocessing.pped_modules_map
        (Preprocess.Per_module.without_instrumentation exes.buildable.preprocess)
        version
    in
    let deps_of module_ =
      let module_ = pp_map module_ in
      immediate_deps_of_module ~options ~obj_dir ~modules:modules_ module_
    in
    let obj_dir = Obj_dir.of_local obj_dir in
    let* scope =
      Scope.DB.find_by_project (Super_context.context sctx) project
    in
    let* modules_ = modules ~obj_dir ~deps_of modules_ in
    let+ requires =
      let* compile_info = Exe_rules.compile_info ~scope exes in
      let open Resolve.Memo.O in
      let* requires = Lib.Compile.direct_requires compile_info in
      if options.with_pps then
        let+ pps = Lib.Compile.pps compile_info in
        pps @ requires
      else Resolve.Memo.return requires
    in
    match Resolve.peek requires with
    | Error () -> None
    | Ok libs ->
      let include_dirs = Obj_dir.all_cmis obj_dir in
      let exe_descr =
        { Descr.Exe.names = List.map ~f:snd exes.names
        ; requires = List.map ~f:uid_of_library libs
        ; modules = modules_
        ; include_dirs
        }
      in
      Some (Descr.Item.Executables exe_descr, Lib.Set.of_list libs)

  (** Builds a workspace item for the provided library object *)
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
          Dir_contents.get sctx ~dir:(Path.as_in_build_dir_exn src_dir)
          >>= Dir_contents.ocaml
          >>| Ml_sources.modules_and_obj_dir ~for_:(Library name)
          >>= fun (modules_, obj_dir_) ->
          let pp_map =
            Staged.unstage
            @@
            let version = (Super_context.context sctx).ocaml.version in
            Preprocessing.pped_modules_map
              (Preprocess.Per_module.without_instrumentation
                 (Lib_info.preprocess info))
              version
          in
          let deps_of module_ =
            immediate_deps_of_module ~options ~obj_dir:obj_dir_
              ~modules:modules_ (pp_map module_)
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

  (** [source_path_is_in_dirs dirs p] tests whether the source path [p] is a
      descendant of some of the provided directory [dirs]. If [dirs = None],
      then it always succeeds. If [dirs = Some l], then a matching directory is
      search in the list [l]. *)
  let source_path_is_in_dirs dirs (p : Path.Source.t) =
    match dirs with
    | None -> true
    | Some dirs ->
      List.exists ~f:(fun dir -> Path.Source.is_descendant p ~of_:dir) dirs

  (** Tests whether a dune file is located in a path that is a descendant of
      some directory *)
  let dune_file_is_in_dirs dirs (dune_file : Dune_file.t) =
    source_path_is_in_dirs dirs dune_file.dir

  (** Tests whether a library is located in a path that is a descendant of some
      directory *)
  let lib_is_in_dirs dirs (lib : Lib.t) =
    source_path_is_in_dirs dirs
      (Path.drop_build_context_exn @@ Lib_info.best_src_dir @@ Lib.info lib)

  (** Builds a workspace item for the root path *)
  let root () = Descr.Item.Root Path.root

  (** Builds a workspace item for the build directory path *)
  let build_ctxt (context : Context.t) : Descr.Item.t =
    Descr.Item.Build_context (Path.build context.build_dir)

  (** Builds a workspace description for the provided dune setup and context *)
  let workspace options
      ({ Dune_rules.Main.conf; contexts = _; scontexts } :
        Dune_rules.Main.build_system) (context : Context.t) dirs :
      Descr.Workspace.t Memo.t =
    let sctx = Context_name.Map.find_exn scontexts context.name in
    let open Memo.O in
    let* dune_files =
      Dune_load.Dune_files.eval conf.dune_files ~context
      >>| List.filter ~f:(dune_file_is_in_dirs dirs)
    in
    let* exes, exe_libs =
      (* the list of workspace items that describe executables, and the list of
         their direct library dependencies *)
      Memo.parallel_map dune_files ~f:(fun (dune_file : Dune_file.t) ->
          Memo.parallel_map dune_file.stanzas ~f:(fun stanza ->
              let dir =
                Path.Build.append_source context.build_dir dune_file.dir
              in
              match stanza with
              | Dune_file.Executables exes ->
                executables sctx ~options ~project:dune_file.project ~dir exes
              | _ -> Memo.return None)
          >>| List.filter_opt)
      >>| List.concat >>| List.split
    in
    let exe_libs =
      (* conflate the dependencies of executables into a single set *)
      Lib.Set.union_all exe_libs
    in
    let* project_libs =
      let ctx = Super_context.context sctx in
      (* the list of libraries declared in the project *)
      Memo.parallel_map conf.projects ~f:(fun project ->
          let* scope = Scope.DB.find_by_project ctx project in
          Scope.libs scope |> Lib.DB.all)
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
end

let term : unit Term.t =
  let+ common = Common.term
  and+ what =
    Arg.(
      value & pos_all string []
      & info [] ~docv:"DIRS"
          ~doc:
            "prints a description of the workspace's structure. If some \
             directories DIRS are provided, then only those directories of the \
             workspace are considered.")
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ format = Describe_common.Format.arg
  and+ lang = Lang.arg
  and+ options = Options.arg in
  let config = Common.init common in
  let dirs =
    let args = "workspace" :: what in
    let parse =
      Dune_lang.Syntax.set Stanza.syntax (Active lang)
      @@
      let open Dune_lang.Decoder in
      fields @@ field "workspace"
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
      Dune_lang.Ast.add_loc ~loc:Loc.none
        (List (List.map args ~f:Dune_lang.atom_or_quoted_string))
    in
    Dune_lang.Decoder.parse parse Univ_map.empty ast
  in
  Scheduler.go ~common ~config @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  let* setup = Memo.run setup in
  let super_context = Import.Main.find_scontext_exn setup ~name:context_name in
  Build_system.run_exn @@ fun () ->
  let context = Super_context.context super_context in
  let open Memo.O in
  (* prefix directories with the workspace root, so that the
     command also works correctly when it is run from a
     subdirectory *)
  Memo.Option.map dirs
    ~f:
      (Memo.List.map ~f:(fun dir ->
           let p =
             Path.Source.(relative root) (Common.prefix_target common dir)
           in
           let s = Path.source p in
           if not @@ Path.exists s then
             User_error.raise
               [ Pp.textf "No such file or directory: %s" (Path.to_string s) ];
           if not @@ Path.is_directory s then
             User_error.raise
               [ Pp.textf "File exists, but is not a directory: %s"
                   (Path.to_string s)
               ];
           Memo.return p))
  >>= Crawl.workspace options setup context
  >>| Sanitize_for_tests.Workspace.sanitize context
  >>| Describe_common.Descr.Workspace.to_dyn options
  >>| Describe_common.Format.print_dyn format

let command =
  let doc =
    "Print a description of the workspace's structure. If some directories \
     DIRS are provided, then only those directories of the workspace are \
     considered."
  in
  let info = Cmd.info ~doc "workspace" in
  Cmd.v info term
