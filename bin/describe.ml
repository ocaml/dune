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

(** Option flags for what to do while crawling the workspace *)
type options =
  { with_deps : bool
        (** whether to compute direct dependencies between modules *)
  ; sanitize_for_tests : bool
        (** whether to sanitize absolute paths of workspace items, and their
            UIDs, to ensure reproducible tests *)
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
  let dyn_path (p : Path.t) : Dyn.t = Dyn.String (Path.to_string p)

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
        [ module_deps ] |> List.filter_map ~f:Fun.id
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
      ; requires : Stdune.Digest.t list
            (** list of direct dependencies to libraries, identified by their
                digests *)
      ; modules : Mod.t list
            (** list of the modules the executable is composed of *)
      ; include_dirs : Path.t list  (** list of include directories *)
      }

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
      ; uid : Stdune.Digest.t  (** digest of the library *)
      ; local : bool  (** whether this library is local *)
      ; requires : Stdune.Digest.t list
            (** list of direct dependendies to libraries, identified by their
                digests *)
      ; source_dir : Path.t
            (** path to the directory that contains the sources of this library *)
      ; modules : Mod.t list
            (** list of the modules the executable is composed of *)
      ; include_dirs : Path.t list  (** list of include directories *)
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

    (** Conversion to the [Dyn.t] type *)
    let to_dyn options : t -> Dyn.t = function
      | Executables exe_descr ->
        Dyn.Variant ("executables", [ Exe.to_dyn options exe_descr ])
      | Library lib_descr ->
        Dyn.Variant ("library", [ Lib.to_dyn options lib_descr ])
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
  open Memo.Build.O

  (** Computes the digest of a name and a source dir *)
  let compute_uid ~(name : Lib_name.t) ~(source_dir : Path.t) =
    Digest.generic (name, Path.to_string source_dir)

  (** Computes the digest of a library *)
  let uid_of_library (lib : Lib.t) : Stdune.Digest.t =
    let name = Lib.name lib
    and source_dir = Lib_info.src_dir (Lib.info lib) in
    compute_uid ~name ~source_dir

  (** Module that computes the direct dependencies between compilation units *)
  module Deps : sig
    val read :
         options:options
      -> use_pp:bool
      -> obj_dir:Path.Build.t Obj_dir.t
      -> modules:Modules.t
      -> Module.t
      -> (Module.t list * Module.t list) Action_builder.t Memo.build
  end = struct
    (** Reads the dependencies of the compilation unit [unit] (for its interface
        and its implementation, if any) *)
    let read_deps_of_unit ~obj_dir ~modules unit =
      let open Action_builder.O in
      let+ deps_for_intf =
        (* compute the dependencies for the interface part *)
        let ml_kind = Dune_util.Ml_kind.Intf in
        Ocamldep.read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit
      and+ deps_for_impl =
        (* compute the dependencies for the implementation part *)
        let ml_kind = Dune_util.Ml_kind.Impl in
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
      if not options.with_deps then
        Memo.Build.return (Action_builder.return no_deps)
      else
        let unit =
          (* For files that are subject to a preprocessing phase, only the
             preprocessed files have an attached ocamldep rule: the source files
             have no such rule attached. In such a case, we refer to the
             preprocessed module instead of the source module. *)
          if use_pp then Module.pped unit else unit
        in
        Memo.Build.return
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
          else read_deps_of_unit ~obj_dir ~modules unit
  end

  (** Builds the description of a module from a module and its object directory *)
  let module_ ~obj_dir ~(deps_for_intf : Module.t list)
      ~(deps_for_impl : Module.t list) (m : Module.t) : Descr.Mod.t =
    let source ml_kind =
      Option.map (Module.source m ~ml_kind) ~f:Module.File.path
    in
    let cmt ml_kind = Obj_dir.Module.cmt_file obj_dir m ~ml_kind in
    Descr.Mod.
      { name = Module.name m
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
            Module.t
         -> (Module.t list * Module.t list) Action_builder.t Memo.Build.t)
      (modules_ : Modules.t) : Descr.Mod.t list Memo.Build.t =
    Modules.fold_no_vlib ~init:(Memo.Build.return [])
      ~f:(fun m macc ->
        let* acc = macc in
        let* deps = deps_of m in
        let+ (deps_for_intf, deps_for_impl), _ =
          Dune_engine.Action_builder.run deps Eager
        in
        module_ ~obj_dir ~deps_for_intf ~deps_for_impl m :: acc)
      modules_

  (** [exes_uses_pp exes] tells whether the compilations units of [exes] are
      subject to a preprocessing phase. *)
  let exes_uses_pp exes =
    let open Preprocess.Per_module in
    List.is_non_empty @@ pps
    @@ without_instrumentation
         exes.Dune_file.Executables.buildable.Dune_file.Buildable.preprocess

  (** Builds a workspace item for the provided executables object *)
  let executables sctx ~options ~project ~dir (exes : Dune_file.Executables.t) :
      Descr.Item.t option Memo.build =
    let first_exe = snd (List.hd exes.Dune_file.Executables.names) in
    let* modules_, obj_dir =
      Dir_contents.get sctx ~dir >>= Dir_contents.ocaml
      >>| Ml_sources.modules_and_obj_dir ~for_:(Exe { first_exe })
    in
    let deps_of =
      let use_pp = exes_uses_pp exes in
      Deps.read ~options ~use_pp ~obj_dir ~modules:modules_
    in
    let obj_dir = Obj_dir.of_local obj_dir in
    let scope = Super_context.find_scope_by_project sctx project in
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
        Descr.Exe.
          { names = List.map ~f:snd exes.names
          ; requires = List.map ~f:uid_of_library libs
          ; modules = modules_
          ; include_dirs
          }
      in
      Some (Descr.Item.Executables exe_descr)

  (** [lib_uses_pp lib] tells whether the compilation units of library [lib] are
      subject to a preprocessing phase. *)
  let lib_uses_pp (lib : Lib.t) : bool Memo.Build.t =
    let+ res = Lib.pps lib in
    match Resolve.peek res with
    | Ok pps -> List.is_non_empty pps
    | Error _ -> assert false

  (** Builds a workspace item for the provided library object *)
  let library sctx ~options (lib : Lib.t) : Descr.Item.t option Memo.build =
    let* requires = Lib.requires lib in
    match Resolve.peek requires with
    | Error () -> Memo.Build.return None
    | Ok requires ->
      let name = Lib.name lib in
      let info = Lib.info lib in
      let src_dir = Lib_info.src_dir info in
      let obj_dir = Lib_info.obj_dir info in
      let+ modules_ =
        if Lib.is_local lib then
          Dir_contents.get sctx ~dir:(Path.as_in_build_dir_exn src_dir)
          >>= Dir_contents.ocaml
          >>| Ml_sources.modules_and_obj_dir ~for_:(Library name)
          >>= fun (modules_, obj_dir_) ->
          let* deps_of =
            let+ use_pp = lib_uses_pp lib in
            Deps.read ~options ~use_pp ~obj_dir:obj_dir_ ~modules:modules_
          in
          modules ~obj_dir ~deps_of modules_
        else Memo.Build.return []
      in
      let include_dirs = Obj_dir.all_cmis obj_dir in
      let lib_descr =
        Descr.Lib.
          { name
          ; uid = uid_of_library lib
          ; local = Lib.is_local lib
          ; requires = List.map requires ~f:uid_of_library
          ; source_dir = src_dir
          ; modules = modules_
          ; include_dirs
          }
      in
      Some (Descr.Item.Library lib_descr)

  (** Builds a workspace description for the provided dune setup and context *)
  let workspace options
      ({ Dune_rules.Main.conf; contexts = _; scontexts } :
        Dune_rules.Main.build_system) (context : Context.t) :
      Descr.Workspace.t Memo.build =
    let sctx = Context_name.Map.find_exn scontexts context.name in
    let* libs =
      Memo.Build.parallel_map conf.projects ~f:(fun project ->
          Super_context.find_scope_by_project sctx project
          |> Scope.libs |> Lib.DB.all)
      >>| fun libs -> libs |> Lib.Set.union_all |> Lib.Set.to_list
    in
    let* libs =
      Memo.Build.parallel_map libs ~f:(fun lib ->
          let+ requires = Lib.requires lib in
          match Resolve.peek requires with
          | Error _ -> []
          | Ok requires -> requires)
      >>| (fun deps ->
            List.concat (libs :: deps) |> Lib.Set.of_list |> Lib.Set.to_list)
      >>= Memo.Build.parallel_map ~f:(library ~options sctx)
      >>| List.filter_map ~f:Fun.id
    in
    let open Memo.Build.O in
    let* dune_files = Dune_load.Dune_files.eval conf.dune_files ~context in
    let* exes =
      Memo.Build.parallel_map dune_files ~f:(fun (dune_file : Dune_file.t) ->
          Memo.Build.parallel_map dune_file.stanzas ~f:(fun stanza ->
              let dir =
                Path.Build.append_source context.build_dir dune_file.dir
              in
              match stanza with
              | Dune_file.Executables exes ->
                executables sctx ~options ~project:dune_file.project ~dir exes
              | _ -> Memo.Build.return None)
          >>| List.filter_map ~f:Fun.id)
      >>| List.concat
    in
    Memo.Build.return (exes @ libs)
end

(** The following module is responsible sanitizing the output of
    [dune describe workspace], so that the absolute paths and the UIDs that
    depend on them are stable for tests. These paths may differ, depending on
    the machine they are run on. *)
module Sanitize_for_tests = struct
  (** State for UID maps: a counter and the map for the registered library
      digests. UID maps are used to associate fresh (but reproducible) UIDs to
      non-local libraries. *)
  type uid_map_state = int * Digest.t Digest.Map.t

  (** State for path maps: a counter and the map for the registered paths. Path
      maps are used to associate fresh (but reproducible) paths to absolute
      paths. *)
  type path_map_state = int * string Path.Map.t

  (** State for analyzing a workspace *)
  type state =
    { required : Digest.Set.t  (** the UIDs of the required libraries *)
    ; defined : Digest.Set.t  (** the UIDs of the defined libraries *)
    ; external_ : Digest.Set.t  (** the UIDs of the external libraries *)
    ; uid_map_state : uid_map_state  (** state for UID maps *)
    ; path_map_state : path_map_state  (** state for path maps *)
    }

  (** Updates the state for UID maps to register a UID. If the UID was
      registered already, the state is unchanged. If the UID is new, then it is
      added to the map with digest of the current value of the counter, and the
      counter is incremented. *)
  let register_in_uid_map ((n, m) as uid_map_state) uid =
    match Digest.Map.add m uid (Digest.generic n) with
    | Ok m' -> (n + 1, m')
    | Error _ -> uid_map_state

  (** Updates the state for path maps to register a path. If the path was
      registered already, the state is unchanged. If the path is new, then it is
      added to the map with the string representation of the current value of
      the counter, and the counter is incremented. *)
  let register_in_path_map ((n, m) as path_map_state) uid =
    match Path.Map.add m uid (string_of_int n) with
    | Ok m' -> (n + 1, m')
    | Error _ -> path_map_state

  module Exe = struct
    open Descr.Exe

    (** Analyzes an executable to detect which libraries it requires or
        declares, which are external, and computes candidates for fresh UIDs and
        fresh paths *)
    let analyze { required; defined; external_; uid_map_state; path_map_state }
        { names = _; requires; modules = _; include_dirs } =
      { required =
          List.fold_left ~f:Stdune.Digest.Set.add ~init:required requires
      ; defined
      ; external_
      ; uid_map_state =
          List.fold_left ~f:register_in_uid_map ~init:uid_map_state requires
      ; path_map_state =
          List.fold_left ~f:register_in_path_map ~init:path_map_state
            include_dirs
      }

    (** Sanitizes an executable, by renaming non-reproducible paths *)
    let sanitize_path rename_path exe =
      { exe with include_dirs = List.map ~f:rename_path exe.include_dirs }

    (** Sanitizes an executable, by renaming the UIDs of required libraries *)
    let sanitize_requires rename_uid exe =
      { exe with requires = List.map ~f:rename_uid exe.requires }
  end

  module Lib = struct
    open Descr.Lib

    (** Analyzes a library to detect which libraries it requires or declares,
        which are external, and computes candidates for fresh UIDs and fresh
        paths *)
    let analyze { required; defined; external_; uid_map_state; path_map_state }
        { name = _
        ; uid
        ; local = _
        ; requires
        ; source_dir
        ; modules = _
        ; include_dirs
        } =
      { required = List.fold_left ~f:Digest.Set.add ~init:required requires
      ; defined = Digest.Set.add defined uid
      ; external_ =
          (if
           (not (Path.is_managed source_dir))
           || List.exists ~f:(Fun.negate Path.is_managed) include_dirs
          then Digest.Set.add external_ uid
          else external_)
      ; uid_map_state =
          List.fold_left ~f:register_in_uid_map
            ~init:(register_in_uid_map uid_map_state uid)
            requires
      ; path_map_state =
          List.fold_left ~f:register_in_path_map
            ~init:(register_in_path_map path_map_state source_dir)
            include_dirs
      }

    (** Sanitizes a library, by renaming its paths and updating its UID if
        necessary. If the UID has changed, add an entry to the UID rename maps
        that serves as an accumulator *)
    let sanitize_path_uid rename_path lib uid_map =
      let old_uid = lib.uid in
      let new_source_dir = rename_path lib.source_dir in
      let new_uid =
        Crawl.compute_uid ~name:lib.name ~source_dir:new_source_dir
      in
      let lib =
        { lib with
          uid = new_uid
        ; source_dir = new_source_dir
        ; include_dirs = List.map ~f:rename_path lib.include_dirs
        }
      in
      let uid_map =
        if Digest.equal old_uid new_uid then uid_map
        else Digest.Map.add_exn uid_map old_uid new_uid
      in
      (lib, uid_map)

    (** Sanitizes the UIDs in the required libraries of a library *)
    let sanitize_requires rename_uid lib =
      { lib with requires = List.map ~f:rename_uid lib.requires }
  end

  module Item = struct
    open Descr.Item

    (** Analyzes a workspace item to detect which libraries it requires or
        declares, which are external, and computes candidates for fresh UIDs and
        fresh paths *)
    let analyze acc = function
      | Executables exe -> Exe.analyze acc exe
      | Library lib -> Lib.analyze acc lib

    (** Sanitizes a workspace item, by renaming non-reproducible UIDs and paths *)
    let sanitize_path_uid rename_path item uid_map =
      match item with
      | Executables exe ->
        (Executables (Exe.sanitize_path rename_path exe), uid_map)
      | Library lib ->
        let lib, uid_map = Lib.sanitize_path_uid rename_path lib uid_map in
        (Library lib, uid_map)

    (** Sanitizes the UIDs in the required libraries of a workspace item *)
    let sanitize_requires rename_uid = function
      | Executables exe -> Executables (Exe.sanitize_requires rename_uid exe)
      | Library lib -> Library (Lib.sanitize_requires rename_uid lib)
  end

  module Workspace = struct
    (** Analyzes a workspace description to detect which libraries it requires
        or declares, which are external, and computes candidates for fresh UIDs
        and fresh paths *)
    let analyze items =
      let init =
        { required = Digest.Set.empty
        ; defined = Digest.Set.empty
        ; external_ = Digest.Set.empty
        ; uid_map_state = (0, Digest.Map.empty)
        ; path_map_state = (0, Path.Map.empty)
        }
      in
      List.fold_left ~f:Item.analyze ~init items

    (** Tries to get OCaml's root path, using the [OPAM_SWITCH_PREFIX]
        environment variable, or using [ocamlc - where], or using
        [ocamlfind ocamlc -where] *)
    let get_ocaml_root () : string option =
      let ( ||| ) o f =
        match o with
        | Some _ as res -> res
        | None -> f ()
      in
      let read_from_env_var var =
        match Unix.getenv var with
        | prefix -> Some prefix
        | exception _ -> None
      in
      let read_line_from_args args =
        match Unix.open_process_args_in args.(0) args with
        | in_chn -> (
          match input_line in_chn with
          | exception _ ->
            close_in in_chn;
            None
          | s -> Some s)
        | exception End_of_file -> None
      in
      Option.map
        ~f:(fun s ->
          (* ensure the path ends with "/" *)
          if String.ends_with ~suffix:Filename.dir_sep s then s
          else s ^ Filename.dir_sep)
        ( read_from_env_var "OPAM_SWITCH_PREFIX" ||| fun () ->
          Option.bind
            ~f:(String.drop_suffix ~suffix:Filename.(concat "lib" "ocaml"))
            ( read_line_from_args [| "ocamlc"; "-where" |] ||| fun () ->
              read_line_from_args [| "ocamlfind"; "ocamlc"; "-where" |] ) )

    (** Sanitizes a workspace description, by renaming non-reproducible UIDs and
        paths *)
    let really_sanitize items =
      let uid_default_replace_map, path_replace_map =
        let { required
            ; defined
            ; external_
            ; uid_map_state = _n, uid_replace_map
            ; path_map_state = _p, path_replace_map
            } =
          analyze items
        in
        let uids_to_sanitize =
          (* the UIDs that are likely to sanitize are the external libraries as
             well as the required libraries that are not defined by the project
             (which may not be listed in the external libraries if the set of
             libraries is not transitively closed) *)
          Digest.Set.(union (diff required defined) external_)
        in
        let uid_default_replace_map =
          (* we only keep the mapping for UIDs that are likely to be unstable:
             the other ones must be left unchanged *)
          Digest.Map.filteri
            ~f:(fun uid _ -> Digest.Set.mem uids_to_sanitize uid)
            uid_replace_map
        in
        (uid_default_replace_map, path_replace_map)
      in
      let rename_path =
        (* the function that renames paths *)
        let default_rename_path =
          (* the default renaming function for paths, that uses the names
             computed during the analysis stage *)
          let prefix = Filename.(concat dir_sep @@ "SANITIZED_ABSOLUTE_PATH") in
          fun path ->
            Path.external_
            @@ Path.External.of_string
                 Filename.(
                   concat prefix (Path.Map.find_exn path_replace_map path))
        in
        match get_ocaml_root () with
        | None -> (* we found no path for OCaml's root *) default_rename_path
        | Some ocaml_root -> (
          function
          (* we have found a path for OCaml's root: let's define the renaming
             function *)
          | Path.External ext_path as path -> (
            (* if the path to rename is an external path, try to find the OCaml
               root inside, and replace it with a fixed string *)
            let s = Path.External.to_string ext_path in
            match String.drop_prefix ~prefix:ocaml_root s with
            | Some s' ->
              (* we have found the OCaml root path: let's replace it with a
                 constant string *)
              Path.external_
              @@ Path.External.of_string
                   Filename.(concat dir_sep @@ concat "OCAML_ROOT" s')
            | None ->
              (* we have not found the OCaml root path: rename using the default
                 renaming function *)
              default_rename_path path)
          | path ->
            (* if the path to rename is not external, it should not be
               changed *)
            path)
      in
      let items, uid_replace_map =
        (* rename the paths in the workspace items, and build the map that
           renames UIDs that have changed: the list of items is now in the
           reversed order *)
        List.fold_left
          ~f:(fun (items, map) item ->
            let new_item, new_map =
              Item.sanitize_path_uid rename_path item map
            in
            (new_item :: items, new_map))
          ~init:([], Digest.Map.empty) items
      in
      let rename_uid uid =
        (* the function that renames UIDs: we use first the UID map that results
           from the path changes, then the UID map that results from the prior
           analysis phase as a fallback *)
        match Digest.Map.find uid_replace_map uid with
        | Some uid' -> uid'
        | None -> (
          match Digest.Map.find uid_default_replace_map uid with
          | Some uid' -> uid'
          | None -> uid)
      in
      (* now, we rename the UIDs in the [requires] field , while reversing the
         list of items, so taht we get back the original ordering *)
      List.rev_map ~f:(Item.sanitize_requires rename_uid) items

    (** Sanitizes a workspace description when options ask to do so, or performs
        no change at all otherwise *)
    let sanitize options items =
      if options.sanitize_for_tests then really_sanitize items else items
  end
end

module Opam_files = struct
  let get () =
    let open Memo.Build.O in
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

  let default = Workspace

  let parse =
    let open Dune_lang.Decoder in
    sum [ ("workspace", return Workspace); ("opam-files", return Opam_files) ]

  let parse ~lang args =
    match args with
    | [] -> default
    | _ ->
      let parse =
        Dune_lang.Syntax.set Dune_engine.Stanza.syntax (Active lang) parse
      in
      let ast =
        Dune_lang.Ast.add_loc ~loc:Loc.none
          (List (List.map args ~f:Dune_lang.atom_or_quoted_string))
      in
      Dune_lang.Decoder.parse parse Univ_map.empty ast

  let describe t options setup context =
    match t with
    | Workspace ->
      let open Memo.Build.O in
      Crawl.workspace options setup context
      >>| Sanitize_for_tests.Workspace.sanitize options
      >>| Descr.Workspace.to_dyn options
    | Opam_files -> Opam_files.get ()
end

module Options = struct
  type t = options

  let arg_with_deps =
    let open Arg in
    value & flag
    & info [ "with-deps" ]
        ~doc:"Whether the dependencies between modules should be printed."

  let arg_sanitize_for_tests =
    let open Arg in
    value & flag
    & info [ "sanitize-for-tests" ]
        ~doc:
          "Sanitize the absolute paths in workspace items, and the associated \
           UIDs, so that the output is reproducible. For use in dune's \
           internal tests only."

  let arg : t Term.t =
    let+ with_deps = arg_with_deps
    and+ sanitize_for_tests = arg_sanitize_for_tests in
    { with_deps; sanitize_for_tests }
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
  let version =
    Dune_lang.Syntax.greatest_supported_version Dune_engine.Stanza.syntax
  in
  Pp.to_fmt Stdlib.Format.std_formatter
    (Dune_engine.Format_dune_lang.pp_top_sexps ~version [ cst ])

let term : unit Term.t =
  let+ common = Common.term
  and+ what =
    Arg.(
      value & pos_all string []
      & info [] ~docv:"STRING"
          ~doc:
            "What to describe. The syntax of this description is tied to the \
             version passed to $(b,--lang)")
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ format = Format.arg
  and+ lang = Lang.arg
  and+ options = Options.arg in
  let config = Common.init common in
  let what = What.parse what ~lang in
  Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      let* setup = Memo.Build.run setup in
      let context = Import.Main.find_context_exn setup ~name:context_name in
      let+ res =
        Build_system.run (fun () -> What.describe what options setup context)
      in
      match res with
      | Error `Already_reported -> ()
      | Ok res -> (
        match format with
        | Csexp -> Csexp.to_channel stdout (Sexp.of_dyn res)
        | Sexp -> print_as_sexp res))

let command : unit Term.t * Term.info = (term, info)
