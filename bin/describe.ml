open Stdune
open Import

(* This command is not yet versioned, but some people are using it in
   non-released tools. If you change the format of the output, please contact:

   - rotor people for "describe workspace"

   - duniverse people for "describe opam-files" *)

let doc = "Describe the workspace."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Describe what is in the current workspace in either human or
        machine readable form.

        By default, this command output a human readable description of
        the current workspace. This output is aimed at human and is not
        suitable for machine processing. In particular, it is not versioned.

        If you want to interpret the output of this command from a program,
        you must use the $(b,--format) option to specify a machine readable
        format as well as the $(b,--lang) option to get a stable output.|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "describe" ~doc ~man

(** whether to sanitize absolute paths of workspace items, and their UIDs, to
    ensure reproducible tests *)
let sanitize_for_tests = ref false

(** Option flags for what to do while crawling the workspace *)
type options =
  { with_deps : bool
        (** whether to compute direct dependencies between modules *)
  }

(** The module [Descr] is a typed representation of the description of a
    workspace, that is provided by the ``dune describe workspace`` command.

    Each sub-module contains a [to_dyn] function, that translates the
    descriptors to a value of type [Dyn.t].

    The typed representation aims at precisely describing the structure of the
    information computed by ``dune describe``, and hopefully make users' life
    easier in decoding the S-expressions into meaningful contents. *)
module Descr = struct
  (** [dyn_path p] converts a path to a value of type [Dyn.t]. Remark: this is
      different from Path.to_dyn, that produces extra tags from a variant
      datatype. *)
  let dyn_path (p : Path.t) : Dyn.t = String (Path.to_string p)

  (** Description of the dependencies of a module *)
  module Mod_deps = struct
    type t =
      { for_intf : Dune_rules.Module_name.t list
            (** direct module dependencies for the interface *)
      ; for_impl : Dune_rules.Module_name.t list
            (** direct module dependencies for the implementation *)
      }

    (** Conversion to the [Dyn.t] type *)
    let to_dyn { for_intf; for_impl } =
      let open Dyn in
      record
        [ ("for_intf", list Dune_rules.Module_name.to_dyn for_intf)
        ; ("for_impl", list Dune_rules.Module_name.to_dyn for_impl)
        ]
  end

  (** Description of modules *)
  module Mod = struct
    type t =
      { name : Dune_rules.Module_name.t  (** name of the module *)
      ; impl : Path.t option  (** path to the .ml file, if any *)
      ; intf : Path.t option  (** path to the .mli file, if any *)
      ; cmt : Path.t option  (** path to the .cmt file, if any *)
      ; cmti : Path.t option  (** path to the .cmti file, if any *)
      ; module_deps : Mod_deps.t  (** direct module dependencies *)
      }

    (** Conversion to the [Dyn.t] type *)
    let to_dyn options { name; impl; intf; cmt; cmti; module_deps } : Dyn.t =
      let open Dyn in
      let optional_fields =
        let module_deps =
          if options.with_deps then
            Some ("module_deps", Mod_deps.to_dyn module_deps)
          else None
        in
        (* we build a list of options, that is later filtered, so that adding
           new optional fields in the future can be done easily *)
        match module_deps with
        | None -> []
        | Some module_deps -> [ module_deps ]
      in
      record
      @@ [ ("name", Dune_rules.Module_name.to_dyn name)
         ; ("impl", option dyn_path impl)
         ; ("intf", option dyn_path intf)
         ; ("cmt", option dyn_path cmt)
         ; ("cmti", option dyn_path cmti)
         ]
      @ optional_fields
  end

  (** Description of executables *)
  module Exe = struct
    type t =
      { names : string list  (** names of the executble *)
      ; requires : Digest.t list
            (** list of direct dependencies to libraries, identified by their
                digests *)
      ; modules : Mod.t list
            (** list of the modules the executable is composed of *)
      ; include_dirs : Path.t list  (** list of include directories *)
      }

    let map_path t ~f = { t with include_dirs = List.map ~f t.include_dirs }

    (** Conversion to the [Dyn.t] type *)
    let to_dyn options { names; requires; modules; include_dirs } : Dyn.t =
      let open Dyn in
      record
        [ ("names", List (List.map ~f:(fun name -> String name) names))
        ; ("requires", Dyn.(list string) (List.map ~f:Digest.to_string requires))
        ; ("modules", list (Mod.to_dyn options) modules)
        ; ("include_dirs", list dyn_path include_dirs)
        ]
  end

  (** Description of libraries *)
  module Lib = struct
    type t =
      { name : Lib_name.t  (** name of the library *)
      ; uid : Digest.t  (** digest of the library *)
      ; local : bool  (** whether this library is local *)
      ; requires : Digest.t list
            (** list of direct dependendies to libraries, identified by their
                digests *)
      ; source_dir : Path.t
            (** path to the directory that contains the sources of this library *)
      ; modules : Mod.t list
            (** list of the modules the executable is composed of *)
      ; include_dirs : Path.t list  (** list of include directories *)
      }

    let map_path t ~f =
      { t with
        source_dir = f t.source_dir
      ; include_dirs = List.map ~f t.include_dirs
      }

    (** Conversion to the [Dyn.t] type *)
    let to_dyn options
        { name; uid; local; requires; source_dir; modules; include_dirs } :
        Dyn.t =
      let open Dyn in
      record
        [ ("name", Lib_name.to_dyn name)
        ; ("uid", String (Digest.to_string uid))
        ; ("local", Bool local)
        ; ("requires", (list string) (List.map ~f:Digest.to_string requires))
        ; ("source_dir", dyn_path source_dir)
        ; ("modules", list (Mod.to_dyn options) modules)
        ; ("include_dirs", (list dyn_path) include_dirs)
        ]
  end

  (** Description of items: executables, or libraries *)
  module Item = struct
    type t =
      | Executables of Exe.t
      | Library of Lib.t

    let map_path t ~f =
      match t with
      | Executables exe -> Executables (Exe.map_path exe ~f)
      | Library lib -> Library (Lib.map_path lib ~f)

    (** Conversion to the [Dyn.t] type *)
    let to_dyn options : t -> Dyn.t = function
      | Executables exe_descr ->
        Variant ("executables", [ Exe.to_dyn options exe_descr ])
      | Library lib_descr ->
        Variant ("library", [ Lib.to_dyn options lib_descr ])
  end

  (** Description of a workspace: a list of items *)
  module Workspace = struct
    type t = Item.t list

    (** Conversion to the [Dyn.t] type *)
    let to_dyn options (items : t) : Dyn.t =
      Dyn.list (Item.to_dyn options) items
  end
end

(** Crawl the workspace to get all the data *)
module Crawl = struct
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

  (** Module that computes the direct dependencies between compilation units *)
  module Deps : sig
    val read :
         options:options
      -> use_pp:bool
      -> obj_dir:Path.Build.t Obj_dir.t
      -> modules:Modules.t
      -> Module.t
      -> (Module.t list * Module.t list) Action_builder.t Memo.t
  end = struct
    (** Reads the dependencies of the compilation unit [unit] (for its interface
        and its implementation, if any) *)
    let read_deps_of_unit ~obj_dir ~modules unit =
      let open Action_builder.O in
      let+ deps_for_intf =
        (* compute the dependencies for the interface part *)
        let ml_kind = Ocaml.Ml_kind.Intf in
        Ocamldep.read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit
      and+ deps_for_impl =
        (* compute the dependencies for the implementation part *)
        let ml_kind = Ocaml.Ml_kind.Impl in
        Ocamldep.read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit
      in
      (deps_for_intf, deps_for_impl)

    (** Tests whether a set of modules is a singleton *)
    let has_single_file modules = Option.is_some @@ Modules.as_singleton modules

    (** [read ~options ~use_pp ~obj_dir ~modules unit] reads the direct
        dependencies of the compilation unit [unit] in the set of modules
        [modules]. The flag [use_pp] indicates whether the compilation unit is
        subject to a preprocessing phase. Two lists are returned. The first list
        contains the direct dependencies of the interface file, whereas the
        second list contains the direct dependencies of the implementation file. *)
    let read ~options ~use_pp ~obj_dir ~modules unit =
      let no_deps = ([], []) in
      match options.with_deps with
      | false -> Memo.return (Action_builder.return no_deps)
      | true -> (
        let unit =
          (* This translation is necessary to handle files that are not in the
             "ocaml" dialect, such as Reason files *)
          Module.ml_source unit
        in
        let unit =
          (* For files that are subject to a preprocessing phase, only the
             preprocessed files have an attached ocamldep rule: the source files
             have no such rule attached. In such a case, we refer to the
             preprocessed module instead of the source module. *)
          if use_pp then Module.pped unit else unit
        in
        Memo.return
        @@
        match Module.kind unit with
        | Module.Kind.Alias ->
          (* TODO: handle Alias modules, that are generated by dune. They are
             currently associated to no ocamldep-related rules. *)
          Action_builder.return no_deps
        | _ ->
          if has_single_file modules then
            (* Single-file modules have no ".ml.d", due to the change in
               https://github.com/ocaml/dune/pull/3847

               FIXME: remove this restriction on singleton modules if/when
               https://github.com/ocaml/dune/pull/4659 is merged *)
            Action_builder.return no_deps
          else read_deps_of_unit ~obj_dir ~modules unit)
  end

  (** Builds the description of a module from a module and its object directory *)
  let module_ ~obj_dir ~(deps_for_intf : Module.t list)
      ~(deps_for_impl : Module.t list) (m : Module.t) : Descr.Mod.t =
    let source ml_kind =
      Option.map (Module.source m ~ml_kind) ~f:Module.File.path
    in
    let cmt ml_kind = Obj_dir.Module.cmt_file obj_dir m ~ml_kind in
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
  let modules ~obj_dir
      ~(deps_of :
         Module.t -> (Module.t list * Module.t list) Action_builder.t Memo.t)
      (modules_ : Modules.t) : Descr.Mod.t list Memo.t =
    Modules.fold_no_vlib ~init:(Memo.return []) modules_ ~f:(fun m macc ->
        let* acc = macc in
        let* deps = deps_of m in
        let+ (deps_for_intf, deps_for_impl), _ =
          Dune_engine.Action_builder.run deps Eager
        in
        module_ ~obj_dir ~deps_for_intf ~deps_for_impl m :: acc)

  (** [module_uses_pp per_module_pp module_] tells whether the module [module_]
      needs some pre-processing stage, according to the per-module information
      [per_module_pp]. The per-module information can be retrieved from the
      library or executable [module_] belongs to. *)
  let module_uses_pp
      (per_module_pp :
        Preprocess.With_instrumentation.t Preprocess.Per_module.t)
      (module_ : Module.t) : bool =
    let open Preprocess in
    match
      Per_module.find (Module.name module_)
        (Per_module.without_instrumentation per_module_pp)
    with
    | No_preprocessing -> false
    | Future_syntax _ | Action _ -> true
    | Pps pps -> not @@ List.is_empty @@ pps.Pps.pps

  (** Builds a workspace item for the provided executables object *)
  let executables sctx ~options ~project ~dir (exes : Dune_file.Executables.t) :
      (Descr.Item.t * Lib.Set.t) option Memo.t =
    let first_exe = snd (List.hd exes.names) in
    let* modules_, obj_dir =
      Dir_contents.get sctx ~dir >>= Dir_contents.ocaml
      >>| Ml_sources.modules_and_obj_dir ~for_:(Exe { first_exe })
    in
    let deps_of module_ =
      let use_pp = module_uses_pp exes.buildable.preprocess module_ in
      Deps.read ~options ~use_pp ~obj_dir ~modules:modules_ module_
    in
    let obj_dir = Obj_dir.of_local obj_dir in
    let* scope =
      Scope.DB.find_by_project (Super_context.context sctx) project
    in
    let* modules_ = modules ~obj_dir ~deps_of modules_ in
    let+ requires =
      let* compile_info = Exe_rules.compile_info ~scope exes in
      Lib.Compile.direct_requires compile_info
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
          let deps_of module_ =
            let use_pp =
              module_uses_pp (Lib_info.preprocess @@ Lib.info lib) module_
            in
            Deps.read ~options ~use_pp ~obj_dir:obj_dir_ ~modules:modules_
              module_
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

  (** [add_transitive_deps libs] returns the union of [libs] and of its
      transitive dependencies *)
  let add_transitive_deps (libs : Lib.t list) =
    (* get the transitive closure using [Lib.closure] *)
    Lib.closure libs ~linking:false
    >>| Resolve.to_result >>| Result.value ~default:[]
    >>| (* then, remove duplicates, and ensure the list is sorted, for
           reproducibility concerns *)
    Lib.Set.of_list >>| Lib.Set.to_list

  (** Builds a workspace description for the provided dune setup and context *)
  let workspace options
      ({ Dune_rules.Main.conf; contexts = _; scontexts } :
        Dune_rules.Main.build_system) (context : Context.t) :
      Descr.Workspace.t Memo.t =
    let sctx = Context_name.Map.find_exn scontexts context.name in
    let open Memo.O in
    let* dune_files = Dune_load.Dune_files.eval conf.dune_files ~context in
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
    in
    let+ libs =
      (* the executables' libraries, and the project's libraries *)
      Lib.Set.union exe_libs project_libs
      |> Lib.Set.to_list |> add_transitive_deps
      >>= Memo.parallel_map ~f:(library ~options sctx)
      >>| List.filter_opt
    in
    exes @ libs
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
        | path ->
          (* if the path to rename is not external, it should not be changed *)
          path
      in
      (* now, we rename the UIDs in the [requires] field , while reversing the
         list of items, so taht we get back the original ordering *)
      List.map ~f:(Descr.Item.map_path ~f:rename_path) items

    (** Sanitizes a workspace description when options ask to do so, or performs
        no change at all otherwise *)
    let sanitize context items =
      if !sanitize_for_tests then really_sanitize context items else items
  end
end

module Opam_files = struct
  let get () =
    let open Memo.O in
    let+ project =
      Dune_engine.Source_tree.root () >>| Dune_engine.Source_tree.Dir.project
    in
    let packages =
      Dune_project.packages project |> Dune_engine.Package.Name.Map.values
    in
    Dyn.List
      (List.map packages ~f:(fun pkg ->
           let opam_file = Path.source (Dune_engine.Package.opam_file pkg) in
           let contents =
             if not (Dune_project.generate_opam_files project) then
               Io.read_file opam_file
             else
               let template_file =
                 Dune_rules.Opam_create.template_file opam_file
               in
               let template =
                 if Path.exists template_file then
                   Some (template_file, Io.read_file template_file)
                 else None
               in
               Dune_rules.Opam_create.generate project pkg ~template
           in
           Dyn.Tuple [ String (Path.to_string opam_file); String contents ]))
end

module Preprocess = struct
  let pp_with_ocamlc sctx project pp_file =
    let open Dune_engine in
    let dump_file =
      Path.map_extension pp_file ~f:(fun ext ->
          let dialect =
            Dialect.DB.find_by_extension (Dune_project.dialects project) ext
          in
          match dialect with
          | None ->
            User_error.raise [ Pp.textf "unsupported extension: %s" ext ]
          | Some (_, (kind : Ocaml.Ml_kind.t)) -> (
            match kind with
            | Intf -> ".cmi.dump"
            | Impl -> ".cmo.dump"))
    in
    let open Fiber.O in
    let+ () =
      Process.run
        ~env:(Super_context.context_env sctx)
        Process.Strict (Super_context.context sctx).ocamlc
        [ "-stop-after"
        ; "parsing"
        ; "-dsource"
        ; Path.to_string pp_file
        ; "-dump-into-file"
        ]
    in
    if not (Path.exists dump_file && Path.is_file dump_file) then
      User_error.raise
        [ Pp.textf "cannot find a dump file: %s" (Path.to_string dump_file) ]
    else Io.cat dump_file;
    Path.unlink_no_err dump_file;
    ()

  let get_pped_file super_context file =
    let open Memo.O in
    let context = Super_context.context super_context in
    let in_build_dir file =
      file |> Path.to_string
      |> Path.Build.relative context.build_dir
      |> Path.build
    in
    let file =
      if String.is_empty file then
        User_error.raise [ Pp.textf "no file is given" ]
      else Path.of_string file |> in_build_dir
    in
    let pp_file = file |> Path.map_extension ~f:(fun ext -> ".pp" ^ ext) in
    Build_system.file_exists pp_file >>= function
    | true ->
      let* _digest = Build_system.build_file pp_file in
      let+ project =
        Dune_engine.Source_tree.root () >>| Dune_engine.Source_tree.Dir.project
      in
      Ok (project, pp_file)
    | false -> (
      Build_system.file_exists file >>= function
      | true ->
        let+ _digest = Build_system.build_file file in
        Error file
      | false ->
        User_error.raise [ Pp.textf "%s does not exist" (Path.to_string file) ])

  let run super_context file =
    let open Memo.O in
    let* result = get_pped_file super_context file in
    match result with
    | Error file -> Io.cat file |> Memo.return
    | Ok (project, file) ->
      pp_with_ocamlc super_context project file
      |> Memo.of_non_reproducible_fiber
end

(* What to describe. To determine what to describe, we convert the positional
   arguments of the command line to a list of atoms and we parse it using the
   regular [Dune_lang.Decoder].

   This way we can reuse all the existing versioning, error reporting, etc...
   machinery. This also allow to easily extend this to arbitrary complex phrases
   without hassle. *)
module What = struct
  type t =
    | Workspace
    | Opam_files
    | Pp of string

  let default = Workspace

  (* The list of command names, their args, their documentation, and their
     parser *)
  let parsers_with_docs :
      (string * string list * string * t Dune_lang.Decoder.t) list =
    let open Dune_lang.Decoder in
    [ ( "workspace"
      , []
      , "prints a description of the workspace's structure"
      , return Workspace )
    ; ( "opam-files"
      , []
      , "prints information about the Opam files that have been discovered"
      , return Opam_files )
    ; ( "pp"
      , [ "FILE" ]
      , "builds a given FILE and prints the preprocessed output"
      , filename >>| fun s -> Pp s )
    ]

  (* The list of documentation strings (one for each command) *)
  let docs =
    List.map parsers_with_docs ~f:(fun (stag, args, doc, _parser) ->
        let command = "$(b," ^ stag ^ ")" in
        let args =
          match args with
          | [] -> " "
          | _ -> " " ^ String.concat ~sep:" " args ^ " "
        in
        let desc = "(" ^ doc ^ ")" in
        command ^ args ^ desc)

  (* The decoder for commands *)
  let parse =
    let open Dune_lang.Decoder in
    sum
    @@ List.map
         ~f:(fun (stag, _args, _doc, parser) -> (stag, parser))
         parsers_with_docs

  let parse ~lang args =
    match args with
    | [] -> default
    | _ ->
      let parse = Dune_lang.Syntax.set Stanza.syntax (Active lang) parse in
      let ast =
        Dune_lang.Ast.add_loc ~loc:Loc.none
          (List (List.map args ~f:Dune_lang.atom_or_quoted_string))
      in
      Dune_lang.Decoder.parse parse Univ_map.empty ast

  let describe t options setup super_context =
    let some = Memo.map ~f:(fun x -> Some x) in
    match t with
    | Opam_files -> Opam_files.get () |> some
    | Workspace ->
      let context = Super_context.context super_context in
      let open Memo.O in
      Crawl.workspace options setup context
      >>| Sanitize_for_tests.Workspace.sanitize context
      >>| Descr.Workspace.to_dyn options
      |> some
    | Pp file ->
      let open Memo.O in
      let+ () = Preprocess.run super_context file in
      None
end

module Options = struct
  type t = options

  let arg_with_deps =
    let open Arg in
    value & flag
    & info [ "with-deps" ]
        ~doc:
          "Whether the dependencies between modules should be printed (for the \
           $(b,workspace) command only)."

  let arg_sanitize_for_tests =
    let open Arg in
    value & flag
    & info [ "sanitize-for-tests" ]
        ~doc:
          "Sanitize the absolute paths in workspace items, and the associated \
           UIDs, so that the output is reproducible. For use in dune's \
           internal tests only (for the $(b,workspace) command only)."

  let arg : t Term.t =
    let+ with_deps = arg_with_deps
    and+ sanitize_for_tests_value = arg_sanitize_for_tests in
    sanitize_for_tests := sanitize_for_tests_value;
    { with_deps }
end

module Format = struct
  type t =
    | Sexp
    | Csexp

  let all = [ ("sexp", Sexp); ("csexp", Csexp) ]

  let arg =
    let doc = Printf.sprintf "$(docv) must be %s" (Arg.doc_alts_enum all) in
    Arg.(value & opt (enum all) Sexp & info [ "format" ] ~docv:"FORMAT" ~doc)
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

let print_as_sexp dyn =
  let rec dune_lang_of_sexp : Sexp.t -> Dune_lang.t = function
    | Atom s -> Dune_lang.atom_or_quoted_string s
    | List l -> List (List.map l ~f:dune_lang_of_sexp)
  in
  let cst =
    dyn |> Sexp.of_dyn |> dune_lang_of_sexp
    |> Dune_lang.Ast.add_loc ~loc:Loc.none
    |> Dune_lang.Cst.concrete
  in
  let version = Dune_lang.Syntax.greatest_supported_version Stanza.syntax in
  Pp.to_fmt Stdlib.Format.std_formatter
    (Dune_lang.Format.pp_top_sexps ~version [ cst ])

let term : unit Term.t =
  let+ common = Common.term
  and+ what =
    Arg.(
      value & pos_all string []
      & info [] ~docv:"STRING"
          ~doc:
            ("What to describe. The syntax of this description is tied to the \
              version passed to $(b,--lang). The currently available commands \
              are the following: "
            ^ String.concat ~sep:", " What.docs
            ^ "."))
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ format = Format.arg
  and+ lang = Lang.arg
  and+ options = Options.arg in
  let config = Common.init common in
  let what = What.parse what ~lang in
  Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      let* setup = Memo.run setup in
      let super_context =
        Import.Main.find_scontext_exn setup ~name:context_name
      in
      let+ res =
        Build_system.run (fun () ->
            What.describe what options setup super_context)
      in
      match res with
      | Error `Already_reported | Ok None -> ()
      | Ok (Some res) -> (
        match format with
        | Csexp -> Csexp.to_channel stdout (Sexp.of_dyn res)
        | Sexp -> print_as_sexp res))

let command : unit Term.t * Term.info = (term, info)
