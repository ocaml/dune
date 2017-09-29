open Import
open Jbuild

module Pset = Path.Set

module Dir_with_jbuild = struct
  type t =
    { src_dir : Path.t
    ; ctx_dir : Path.t
    ; stanzas : Stanzas.t
    ; scope   : Scope.t
    }
end

module External_dir = struct
  (* Files in the directory, grouped by extension *)
  type t = Path.t list String_map.t

  let create ~dir : t =
    match Path.readdir dir with
    | exception _ -> String_map.empty
    | files ->
      List.map files ~f:(fun fn -> Filename.extension fn, Path.relative dir fn)
      |> String_map.of_alist_multi
  (* CR-someday jdimino: when we can have dynamic targets:

     {[
       |> String_map.mapi ~f:(fun ext files ->
         lazy (
           let alias =
             Alias.make ~dir:Path.root (sprintf "external-files-%s%s" hash ext)
           in
           Alias.add_deps aliases alias files;
           alias
         ))
     ]}
  *)

  let files t ~ext = String_map.find_default ext t ~default:[]
end

type t =
  { context                                 : Context.t
  ; libs                                    : Lib_db.t
  ; stanzas                                 : Dir_with_jbuild.t list
  ; packages                                : Package.t String_map.t
  ; aliases                                 : Alias.Store.t
  ; file_tree                               : File_tree.t
  ; artifacts                               : Artifacts.t
  ; mutable rules                           : Build_interpret.Rule.t list
  ; stanzas_to_consider_for_install         : (Path.t * Stanza.t) list
  ; mutable known_targets_by_src_dir_so_far : String_set.t Path.Map.t
  ; libs_vfile                              : (module Vfile_kind.S with type t = Lib.t list)
  ; cxx_flags                               : string list
  ; vars                                    : string String_map.t
  ; ppx_dir                                 : Path.t
  ; ppx_drivers                             : (string, Path.t) Hashtbl.t
  ; external_dirs                           : (Path.t, External_dir.t) Hashtbl.t
  ; chdir                                   : (Action.t, Action.t) Build.t
  }

let context t = t.context
let aliases t = t.aliases
let stanzas t = t.stanzas
let packages t = t.packages
let artifacts t = t.artifacts
let file_tree t = t.file_tree
let rules t = t.rules
let stanzas_to_consider_for_install t = t.stanzas_to_consider_for_install
let cxx_flags t = t.cxx_flags

let expand_var_no_root t var = String_map.find var t.vars

let get_external_dir t ~dir =
  Hashtbl.find_or_add t.external_dirs dir ~f:(fun dir ->
    External_dir.create ~dir)

let expand_vars t ~scope ~dir s =
  String_with_vars.expand s ~f:(fun _loc -> function
  | "ROOT" -> Some (Path.reach ~from:dir t.context.build_dir)
  | "SCOPE_ROOT" ->
    Some (Path.reach ~from:dir (Path.append t.context.build_dir scope.Scope.root))
  | var -> String_map.find var t.vars)

let resolve_program_internal t ?hint ?(in_the_tree=true) bin =
  Artifacts.binary t.artifacts ?hint ~in_the_tree bin

let resolve_program t ?hint ?in_the_tree bin =
  match resolve_program_internal t ?hint ?in_the_tree bin with
  | Error fail -> Build.Prog_spec.Dyn (fun _ -> fail.fail ())
  | Ok    path -> Build.Prog_spec.Dep path

let create
      ~(context:Context.t)
      ~aliases
      ~dirs_with_dot_opam_files
      ~file_tree
      ~packages
      ~stanzas
      ~filter_out_optional_stanzas_with_missing_deps
  =
  let stanzas =
    List.map stanzas
      ~f:(fun (dir, scope, stanzas) ->
        { Dir_with_jbuild.
          src_dir = dir
        ; ctx_dir = Path.append context.build_dir dir
        ; stanzas
        ; scope
        })
  in
  let internal_libraries =
    List.concat_map stanzas ~f:(fun { ctx_dir;  stanzas; _ } ->
      List.filter_map stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib -> Some (ctx_dir, lib)
        | _ -> None))
  in
  let dirs_with_dot_opam_files =
    Pset.elements dirs_with_dot_opam_files
    |> List.map ~f:(Path.append context.build_dir)
    |> Pset.of_list
  in
  let libs =
    Lib_db.create context.findlib internal_libraries
      ~dirs_with_dot_opam_files
  in
  let stanzas_to_consider_for_install =
    if filter_out_optional_stanzas_with_missing_deps then
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; _ } ->
        List.filter_map stanzas ~f:(function
          | Library _ -> None
          | stanza    -> Some (ctx_dir, stanza)))
      @ List.map
          (Lib_db.internal_libs_without_non_installable_optional_ones libs)
          ~f:(fun (dir, lib) -> (dir, Stanza.Library lib))
    else
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; _ } ->
        List.map stanzas ~f:(fun s -> (ctx_dir, s)))
  in
  let module Libs_vfile =
    Vfile_kind.Make_full
      (struct type t = Lib.t list end)
      (struct
        open Sexp.To_sexp
        let t _dir l = list string (List.map l ~f:Lib.best_name)
      end)
      (struct
        open Sexp.Of_sexp
        let t dir sexp =
          List.map (list string sexp) ~f:(Lib_db.find_exn libs ~from:dir)
      end)
  in
  let artifacts =
    Artifacts.create context stanzas ~f:(fun (d : Dir_with_jbuild.t) ->
      d.stanzas)
  in
  let cxx_flags =
    String.extract_blank_separated_words context.ocamlc_cflags
    |> List.filter ~f:(fun s -> not (String.is_prefix s ~prefix:"-std="))
  in
  let vars =
    let ocamlopt =
      match context.ocamlopt with
      | None -> Path.relative context.ocaml_bin "ocamlopt"
      | Some p -> p
    in
    let make =
      match Bin.make with
      | None   -> "make"
      | Some p -> Path.to_string p
    in
    [ "-verbose"       , "" (*"-verbose";*)
    ; "CPP"            , sprintf "%s %s -E" context.c_compiler context.ocamlc_cflags
    ; "PA_CPP"         , sprintf "%s %s -undef -traditional -x c -E" context.c_compiler
                           context.ocamlc_cflags
    ; "CC"             , sprintf "%s %s" context.c_compiler context.ocamlc_cflags
    ; "CXX"            , String.concat ~sep:" " (context.c_compiler :: cxx_flags)
    ; "ocaml_bin"      , Path.to_string context.ocaml_bin
    ; "OCAML"          , Path.to_string context.ocaml
    ; "OCAMLC"         , Path.to_string context.ocamlc
    ; "OCAMLOPT"       , Path.to_string ocamlopt
    ; "ocaml_version"  , context.version
    ; "ocaml_where"    , Path.to_string context.stdlib_dir
    ; "ARCH_SIXTYFOUR" , string_of_bool context.arch_sixtyfour
    ; "MAKE"           , make
    ; "null"           , Path.to_string Config.dev_null
    ]
    |> String_map.of_alist
    |> function
    | Ok x -> x
    | Error _ -> assert false
  in
  { context
  ; libs
  ; stanzas
  ; packages
  ; aliases
  ; file_tree
  ; rules = []
  ; stanzas_to_consider_for_install
  ; known_targets_by_src_dir_so_far = Path.Map.empty
  ; libs_vfile = (module Libs_vfile)
  ; artifacts
  ; cxx_flags
  ; vars
  ; ppx_drivers = Hashtbl.create 32
  ; ppx_dir = Path.relative context.build_dir ".ppx"
  ; external_dirs = Hashtbl.create 1024
  ; chdir = Build.arr (fun (action : Action.t) ->
      match action with
      | Chdir _ -> action
      | _ -> Chdir (context.build_dir, action))
  }

let add_rule t ?sandbox ?fallback ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  let rule =
    Build_interpret.Rule.make ?sandbox ?fallback ?locks ?loc
      ~context:t.context build
  in
  t.rules <- rule :: t.rules;
  t.known_targets_by_src_dir_so_far <-
    List.fold_left rule.targets ~init:t.known_targets_by_src_dir_so_far
      ~f:(fun acc target ->
        match Path.extract_build_context (Build_interpret.Target.path target) with
        | None -> acc
        | Some (_, path) ->
          let dir = Path.parent path in
          let fn = Path.basename path in
          let files =
            match Path.Map.find dir acc with
            | None -> String_set.singleton fn
            | Some set -> String_set.add fn set
          in
          Path.Map.add acc ~key:dir ~data:files)

let add_rules t ?sandbox builds =
  List.iter builds ~f:(add_rule t ?sandbox)

let sources_and_targets_known_so_far t ~src_path =
  let sources =
    match File_tree.find_dir t.file_tree src_path with
    | None -> String_set.empty
    | Some dir -> File_tree.Dir.files dir
  in
  match Path.Map.find src_path t.known_targets_by_src_dir_so_far with
  | None -> sources
  | Some set -> String_set.union sources set


module Libs = struct
  open Build.O
  open Lib_db

  let find t ~from name = find t.libs ~from name

  let vrequires t ~dir ~item =
    let fn = Path.relative dir (item ^ ".requires.sexp") in
    Build.Vspec.T (fn, t.libs_vfile)

  let load_requires t ~dir ~item =
    Build.vpath (vrequires t ~dir ~item)

  let vruntime_deps t ~dir ~item =
    let fn = Path.relative dir (item ^ ".runtime-deps.sexp") in
    Build.Vspec.T (fn, t.libs_vfile)

  let load_runtime_deps t ~dir ~item =
    Build.vpath (vruntime_deps t ~dir ~item)

  let with_fail ~fail build =
    match fail with
    | None -> build
    | Some f -> Build.fail f >>> build

  let closure t ~dir ~dep_kind lib_deps =
    let internals, externals, fail = Lib_db.interpret_lib_deps t.libs ~dir lib_deps in
    with_fail ~fail
      (Build.record_lib_deps ~dir ~kind:dep_kind lib_deps
       >>>
       Build.all
         (List.map internals ~f:(fun ((dir, lib) : Lib.Internal.t) ->
            load_requires t ~dir ~item:lib.name))
       >>^ (fun internal_deps ->
         let externals =
           Findlib.closure externals
             ~required_by:dir
             ~local_public_libs:(local_public_libs t.libs)
           |> List.map ~f:(fun pkg -> Lib.External pkg)
         in
         Lib.remove_dups_preserve_order
           (List.concat (externals :: internal_deps) @
            List.map internals ~f:(fun x -> Lib.Internal x))))

  let closed_ppx_runtime_deps_of t ~dir ~dep_kind lib_deps =
    let internals, externals, fail = Lib_db.interpret_lib_deps t.libs ~dir lib_deps in
    with_fail ~fail
      (Build.record_lib_deps ~dir ~kind:dep_kind lib_deps
       >>>
       Build.all
         (List.map internals ~f:(fun ((dir, lib) : Lib.Internal.t) ->
            load_runtime_deps t ~dir ~item:lib.name))
       >>^ (fun libs ->
         let externals =
           Findlib.closed_ppx_runtime_deps_of externals
             ~required_by:dir
             ~local_public_libs:(local_public_libs t.libs)
           |> List.map ~f:(fun pkg -> Lib.External pkg)
         in
         Lib.remove_dups_preserve_order (List.concat (externals :: libs))))

  let lib_is_available t ~from name = lib_is_available t.libs ~from name

  let add_select_rules t ~dir lib_deps =
    List.iter (Lib_db.resolve_selects t.libs ~from:dir lib_deps) ~f:(fun { dst_fn; src_fn } ->
      let src = Path.relative dir src_fn in
      let dst = Path.relative dir dst_fn in
      add_rule t
        (Build.path src
         >>>
         Build.action ~targets:[dst]
           (Copy_and_add_line_directive (src, dst))))

  let real_requires t ~dir ~dep_kind ~item ~libraries ~preprocess ~virtual_deps =
    let all_pps =
      List.map (Preprocess_map.pps preprocess) ~f:Pp.to_string
    in
    let vrequires = vrequires t ~dir ~item in
    add_rule t
      (Build.record_lib_deps ~dir ~kind:dep_kind (List.map virtual_deps ~f:Lib_dep.direct)
       >>>
       Build.fanout
         (closure t ~dir ~dep_kind libraries)
         (closed_ppx_runtime_deps_of t ~dir ~dep_kind
            (List.map all_pps ~f:Lib_dep.direct))
       >>>
       Build.arr (fun (libs, rt_deps) ->
         Lib.remove_dups_preserve_order (libs @ rt_deps))
       >>>
       Build.store_vfile vrequires);
    Build.vpath vrequires

  let requires t ~dir ~dep_kind ~item ~libraries ~preprocess ~virtual_deps
        ~has_dot_merlin =
    let real_requires =
      real_requires t ~dir ~dep_kind ~item ~libraries ~preprocess ~virtual_deps
    in
    let requires =
      if t.context.merlin && has_dot_merlin then
        (* We don't depend on the dot_merlin directly, otherwise everytime it changes we
           would have to rebuild everything.

           .merlin-exists depends on the .merlin and is an empty file. Depending on it
           forces the generation of the .merlin but not recompilation when it
           changes. Maybe one day we should add [Build.path_exists] to do the same in
           general. *)
        Build.path (Path.relative dir ".merlin-exists")
        >>>
        real_requires
      else
        real_requires
    in
    (requires, real_requires)

  let setup_runtime_deps t ~dir ~dep_kind ~item ~libraries ~ppx_runtime_libraries =
    let vruntime_deps = vruntime_deps t ~dir ~item in
    add_rule t
      (Build.fanout
         (closure t ~dir ~dep_kind (List.map ppx_runtime_libraries ~f:Lib_dep.direct))
         (closed_ppx_runtime_deps_of t ~dir ~dep_kind libraries)
       >>>
       Build.arr (fun (rt_deps, rt_deps_of_deps) ->
         Lib.remove_dups_preserve_order (rt_deps @ rt_deps_of_deps))
       >>>
       Build.store_vfile vruntime_deps)

  let lib_files_alias ((dir, lib) : Lib.Internal.t) ~ext =
    Alias.make (sprintf "lib-%s%s-all" lib.name ext) ~dir

  let setup_file_deps_alias t lib ~ext files =
    Alias.add_deps t.aliases (lib_files_alias lib ~ext) files

  let setup_file_deps_group_alias t lib ~exts =
    setup_file_deps_alias t lib
      ~ext:(String.concat exts ~sep:"-and-")
      (List.map exts ~f:(fun ext -> Alias.file (lib_files_alias lib ~ext)))

  let file_deps t ~ext =
    Build.dyn_paths (Build.arr (fun libs ->
      List.fold_left libs ~init:[] ~f:(fun acc (lib : Lib.t) ->
        match lib with
        | External pkg -> begin
            List.rev_append
              (External_dir.files (get_external_dir t ~dir:pkg.dir) ~ext)
              acc
          end
        | Internal lib ->
          Alias.file (lib_files_alias lib ~ext) :: acc)))

  let static_file_deps ~ext lib =
    Alias.dep (lib_files_alias lib ~ext)
end

module Deps = struct
  open Build.O
  open Dep_conf

  let dep t ~scope ~dir = function
    | File  s ->
      let path = Path.relative dir (expand_vars t ~scope ~dir s) in
      Build.path path
      >>^ fun () -> [path]
    | Alias s ->
      Alias.dep (Alias.make ~dir (expand_vars t ~scope ~dir s))
      >>^ fun () -> []
    | Alias_rec s ->
      Alias.dep_rec ~loc:(String_with_vars.loc s) ~file_tree:t.file_tree
        (Alias.make ~dir (expand_vars t ~scope ~dir s))
      >>^ fun () -> []
    | Glob_files s -> begin
        let path = Path.relative dir (expand_vars t ~scope ~dir s) in
        match Glob_lexer.parse_string (Path.basename path) with
        | Ok re ->
          let dir = Path.parent path in
          Build.paths_glob ~dir (Re.compile re)
        | Error (_pos, msg) ->
          let loc = String_with_vars.loc s in
          Loc.fail loc "invalid glob: %s" msg
      end
    | Files_recursively_in s ->
      let path = Path.relative dir (expand_vars t ~scope ~dir s) in
      Build.files_recursively_in ~dir:path ~file_tree:t.file_tree
      >>^ Pset.elements

  let interpret t ~scope ~dir l =
    Build.all (List.map l ~f:(dep t ~scope ~dir))
    >>^ List.concat
end

module Pkg_version = struct
  open Build.O

  module V = Vfile_kind.Make(struct type t = string option end)
      (functor (C : Sexp.Combinators) -> struct
        let t = C.option C.string
      end)

  let spec sctx (p : Package.t) =
    let fn =
      Path.relative (Path.append sctx.context.build_dir p.path)
        (sprintf "%s.version.sexp" p.name)
    in
    Build.Vspec.T (fn, (module V))

  let read sctx p = Build.vpath (spec sctx p)

  let set sctx p get =
    let spec = spec sctx p in
    add_rule sctx (get >>> Build.store_vfile spec);
    Build.vpath spec
end

let parse_bang var : Action.Var_expansion.Concat_or_split.t * string =
  let len = String.length var in
  if len > 0 && var.[0] = '!' then
    (Split, String.sub var ~pos:1 ~len:(len - 1))
  else
    (Concat, var)

module Action = struct
  open Build.O
  module U = Action.Unexpanded

  type targets =
    | Static of Path.t list
    | Infer

  type resolved_forms =
    { (* Failed resolutions *)
      mutable failures  : fail list
    ; (* All "name" for ${lib:name:...}/${lib-available:name} forms *)
      mutable lib_deps  : Build.lib_deps
    ; (* Static deps from ${...} variables. For instance ${exe:...} *)
      mutable sdeps     : Pset.t
    ; (* Dynamic deps from ${...} variables. For instance ${read:...} *)
      mutable ddeps     : (unit, Action.Var_expansion.t) Build.t String_map.t
    }

  let add_lib_dep acc lib kind =
    acc.lib_deps <- String_map.add acc.lib_deps ~key:lib ~data:kind

  let add_fail acc fail =
    acc.failures <- fail :: acc.failures;
    None

  let add_ddep acc ~key dep =
    acc.ddeps <- String_map.add acc.ddeps ~key ~data:dep;
    None

  let path_exp path = Action.Var_expansion.Paths   ([path], Concat)
  let str_exp  path = Action.Var_expansion.Strings ([path], Concat)

  (* Static expansion that creates a dependency on the expanded path *)
  let static_dep_exp acc path =
    acc.sdeps <- Pset.add path acc.sdeps;
    Some (path_exp path)

  let expand_step1 sctx ~dir ~dep_kind ~scope ~targets_written_by_user t =
    let acc =
      { failures  = []
      ; lib_deps  = String_map.empty
      ; sdeps     = Pset.empty
      ; ddeps     = String_map.empty
      }
    in
    let t =
      U.partial_expand dir t ~f:(fun loc key ->
        let module A = Artifacts in
        let open Action.Var_expansion in
        let cos, var = parse_bang key in
        match String.lsplit2 var ~on:':' with
        | Some ("path-no-dep", s) -> Some (path_exp (Path.relative dir s))
        | Some ("exe"     , s) -> static_dep_exp acc (Path.relative dir s)
        | Some ("path"    , s) -> static_dep_exp acc (Path.relative dir s)
        | Some ("bin"     , s) -> begin
            match A.binary (artifacts sctx) s with
            | Ok path -> static_dep_exp acc path
            | Error fail -> add_fail acc fail
          end
        (* "findlib" for compatibility with Jane Street packages which are not yet updated
           to convert "findlib" to "lib" *)
        | Some (("lib"|"findlib"), s) -> begin
            let lib_dep, res = A.file_of_lib (artifacts sctx) ~loc ~from:dir s in
            add_lib_dep acc lib_dep dep_kind;
            match res with
            | Ok path -> static_dep_exp acc path
            | Error fail -> add_fail acc fail
          end
        | Some ("libexec" , s) -> begin
            let lib_dep, res = A.file_of_lib (artifacts sctx) ~loc ~from:dir s in
            add_lib_dep acc lib_dep dep_kind;
            match res with
            | Error fail -> add_fail acc fail
            | Ok path ->
              if not Sys.win32 || Filename.extension s = ".exe" then begin
                static_dep_exp acc path
              end else begin
                let path_exe = Path.extend_basename path ~suffix:".exe" in
                let dep =
                  Build.if_file_exists path_exe
                    ~then_:(Build.path path_exe >>^ fun _ -> path_exp path_exe)
                    ~else_:(Build.path path     >>^ fun _ -> path_exp path)
                in
                add_ddep acc ~key dep
              end
          end
        | Some ("lib-available", lib) ->
          add_lib_dep acc lib Optional;
          Some (str_exp (string_of_bool (Libs.lib_is_available sctx ~from:dir lib)))
        | Some ("version", s) -> begin
            match Scope.resolve scope s with
            | Ok p ->
              let x =
                Pkg_version.read sctx p >>^ function
                | None   -> Strings ([""], Concat)
                | Some s -> Strings ([s],  Concat)
              in
              add_ddep acc ~key x
            | Error s ->
              add_fail acc { fail = fun () -> Loc.fail loc "%s" s }
          end
        | Some ("read", s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.contents path
              >>^ fun s -> Strings ([s], cos)
            in
            add_ddep acc ~key data
          end
        | Some ("read-lines", s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.lines_of path
              >>^ fun l -> Strings (l, cos)
            in
            add_ddep acc ~key data
          end
        | Some ("read-strings", s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.strings path
              >>^ fun l -> Strings (l, cos)
            in
            add_ddep acc ~key data
          end
        | _ ->
          match var with
          | "ROOT" -> Some (path_exp sctx.context.build_dir)
          | "SCOPE_ROOT" -> Some (path_exp (Path.append sctx.context.build_dir scope.root))
          | "@" -> begin
              match targets_written_by_user with
              | Infer -> Loc.fail loc "You cannot use ${@} with inferred rules."
              | Static l -> Some (Paths (l, cos))
            end
          | _ ->
            match expand_var_no_root sctx var with
            | Some s -> Some (str_exp s)
            | None -> None)
    in
    (t, acc)

  let expand_step2 ~dir ~dynamic_expansions ~deps_written_by_user t =
    let open Action.Var_expansion in
    U.Partial.expand dir t ~f:(fun _loc key ->
      match String_map.find key dynamic_expansions with
      | Some _ as opt -> opt
      | None ->
        let cos, var = parse_bang key in
        match var with
        | "<" ->
          Some
            (match deps_written_by_user with
             | [] ->
               (* CR-someday jdimino: this should be an error *)
               Strings ([""], cos)
             | dep :: _ ->
               Paths ([dep], cos))
        | "^" -> Some (Paths (deps_written_by_user, cos))
        | _ -> None)

  let run sctx t ~dir ~dep_kind ~targets:targets_written_by_user ~scope
    : (Path.t list, Action.t) Build.t =
    let t, forms =
      expand_step1 sctx t ~dir ~dep_kind ~scope
        ~targets_written_by_user
    in
    let { Action.Infer.Outcome. deps; targets } =
      match targets_written_by_user with
      | Infer -> Action.Infer.partial t ~all_targets:true
      | Static targets_written_by_user ->
        let targets_written_by_user = Pset.of_list targets_written_by_user in
        let { Action.Infer.Outcome. deps; targets } =
          Action.Infer.partial t ~all_targets:false
        in
        (* CR-someday jdimino: should this be an error or not?

           It's likely that what we get here is what the user thinks of as temporary
           files, even though they might conflict with actual targets. We need to tell
           jbuilder about such things, so that it can report better errors.

           {[
             let missing = Pset.diff targets targets_written_by_user in
             if not (Pset.is_empty missing) then
               Loc.warn (Loc.in_file (Utils.jbuild_name_in ~dir))
                 "Missing targets in user action:\n%s"
                 (List.map (Pset.elements missing) ~f:(fun target ->
                    sprintf "- %s" (Utils.describe_target target))
                  |> String.concat ~sep:"\n");
           ]}
        *)
        { deps; targets = Pset.union targets targets_written_by_user }
    in
    let targets = Pset.elements targets in
    List.iter targets ~f:(fun target ->
      if Path.parent target <> dir then
        Loc.fail (Loc.in_file (Utils.jbuild_name_in ~dir))
          "A rule in this jbuild has targets in a different directory \
           than the current one, this is not allowed by Jbuilder at the moment:\n%s"
          (List.map targets ~f:(fun target ->
             sprintf "- %s" (Utils.describe_target target))
           |> String.concat ~sep:"\n"));
    let build =
      Build.record_lib_deps_simple ~dir forms.lib_deps
      >>>
      Build.path_set deps
      >>>
      Build.path_set forms.sdeps
      >>>
      Build.arr (fun paths -> ((), paths))
      >>>
      let ddeps = String_map.bindings forms.ddeps in
      Build.first (Build.all (List.map ddeps ~f:snd))
      >>^ (fun (vals, deps_written_by_user) ->
        let dynamic_expansions =
          List.fold_left2 ddeps vals ~init:String_map.empty ~f:(fun acc (var, _) value ->
            String_map.add acc ~key:var ~data:value)
        in
        let unresolved =
          expand_step2 t ~dir ~dynamic_expansions ~deps_written_by_user
        in
        Action.Unresolved.resolve unresolved ~f:(fun prog ->
          match resolve_program_internal sctx prog with
          | Ok path    -> path
          | Error fail -> fail.fail ()))
      >>>
      Build.dyn_paths (Build.arr (fun action ->
        let { Action.Infer.Outcome.deps; targets = _ } =
          Action.Infer.infer action
        in
        Pset.elements deps))
      >>>
      Build.action_dyn () ~dir ~targets
    in
    match forms.failures with
    | [] -> build
    | fail :: _ -> Build.fail fail >>> build
end

module PP = struct
  open Build.O

  let pp_fname fn =
    let fn, ext = Filename.split_extension fn in
    (* We need to to put the .pp before the .ml so that the compiler realises that
       [foo.pp.mli] is the interface for [foo.pp.ml] *)
    fn ^ ".pp" ^ ext

  let pped_module ~dir (m : Module.t) ~f =
    let ml_pp_fname = pp_fname m.impl.name in
    f Ml_kind.Impl (Path.relative dir m.impl.name) (Path.relative dir ml_pp_fname);
    let intf =
      Option.map m.intf ~f:(fun intf ->
        let pp_fname = pp_fname intf.name in
        f Intf (Path.relative dir intf.name) (Path.relative dir pp_fname);
        {intf with name = pp_fname})
    in
    { m with
      impl = { m.impl with name = ml_pp_fname }
    ; intf
    }

  let migrate_driver_main = "ocaml-migrate-parsetree.driver-main"

  let build_ppx_driver sctx ~dir ~dep_kind ~target pp_names ~driver =
    let ctx = sctx.context in
    let mode = Context.best_mode ctx in
    let compiler = Option.value_exn (Context.compiler ctx mode) in
    let pp_names = pp_names @ [migrate_driver_main] in
    let libs =
      Libs.closure sctx ~dir ~dep_kind (List.map pp_names ~f:Lib_dep.direct)
    in
    let libs =
      (* Put the driver back at the end, just before migrate_driver_main *)
      match driver with
      | None -> libs
      | Some driver ->
        libs >>^ fun libs ->
        let is_driver name = name = driver || name = migrate_driver_main in
        let libs, drivers =
          List.partition_map libs ~f:(fun lib ->
            if (match lib with
              | External pkg -> is_driver pkg.name
              | Internal (_, lib) ->
                is_driver lib.name ||
                match lib.public with
                | None -> false
                | Some { name; _ } -> is_driver name)
            then
              Inr lib
            else
              Inl lib)
        in
        let user_driver, migrate_driver =
          List.partition_map drivers ~f:(fun lib ->
            if Lib.best_name lib = migrate_driver_main then
              Inr lib
            else
              Inl lib)
        in
        libs @ user_driver @ migrate_driver
    in
    (* Provide a better error for migrate_driver_main given that this is an implicit
       dependency *)
    let libs =
      match Libs.find sctx ~from:dir migrate_driver_main with
      | None ->
        Build.fail { fail = fun () ->
          die "@{<error>Error@}: I couldn't find '%s'.\n\
               I need this library in order to use ppx rewriters.\n\
               See the manual for details.\n\
               Hint: opam install ocaml-migrate-parsetree"
            migrate_driver_main
        }
        >>>
        libs
      | Some _ ->
        libs
    in
    add_rule sctx
      (libs
       >>>
       Build.dyn_paths (Build.arr (Lib.archive_files ~mode ~ext_lib:ctx.ext_lib))
       >>>
       Build.run ~context:ctx (Dep compiler)
         [ A "-o" ; Target target
         ; Dyn (Lib.link_flags ~mode)
         ])

  let get_ppx_driver sctx pps ~dir ~dep_kind =
    let driver, names =
      match List.rev_map pps ~f:Pp.to_string with
      | [] -> (None, [])
      | driver :: rest ->
        (Some driver, List.sort rest ~cmp:String.compare @ [driver])
    in
    let key =
      match names with
      | [] -> "+none+"
      | _  -> String.concat names ~sep:"+"
    in
    match Hashtbl.find sctx.ppx_drivers key with
    | Some x -> x
    | None ->
      let ppx_dir = Path.relative sctx.ppx_dir key in
      let exe = Path.relative ppx_dir "ppx.exe" in
      build_ppx_driver sctx names ~dir ~dep_kind ~target:exe ~driver;
      Hashtbl.add sctx.ppx_drivers ~key ~data:exe;
      exe

  let target_var = String_with_vars.virt_var __POS__ "@"
  let root_var   = String_with_vars.virt_var __POS__ "ROOT"

  let cookie_library_name lib_name =
    match lib_name with
    | None -> []
    | Some name -> ["--cookie"; sprintf "library-name=%S" name]

  (* Generate rules for the reason modules in [modules] and return a
     a new module with only OCaml sources *)
  let setup_reason_rules sctx ~dir (m : Module.t) =
    let ctx = sctx.context in
    let refmt = resolve_program sctx "refmt" ~hint:"opam install reason" in
    let rule src target =
      let src_path = Path.relative dir src in
      Build.run ~context:ctx refmt
        [ A "--print"
        ; A "binary"
        ; Dep src_path ]
        ~stdout_to:(Path.relative dir target) in
    let impl =
      match m.impl.syntax with
      | OCaml -> m.impl
      | Reason ->
        let ml = Module.File.to_ocaml m.impl in
        add_rule sctx (rule m.impl.name ml.name);
        ml in
    let intf =
      Option.map m.intf ~f:(fun f ->
        match f.syntax with
        | OCaml -> f
        | Reason ->
          let mli = Module.File.to_ocaml f in
          add_rule sctx (rule f.name mli.name);
          mli) in
    { m with impl ; intf }

  (* Generate rules to build the .pp files and return a new module map where all filenames
     point to the .pp files *)
  let pped_modules sctx ~dir ~dep_kind ~modules ~preprocess ~preprocessor_deps ~lib_name
        ~scope =
    let preprocessor_deps =
      Build.memoize "preprocessor deps"
        (Deps.interpret sctx ~scope ~dir preprocessor_deps)
    in
    String_map.map modules ~f:(fun (m : Module.t) ->
      let m = setup_reason_rules sctx ~dir m in
      match Preprocess_map.find m.name preprocess with
      | No_preprocessing -> m
      | Action action ->
        pped_module m ~dir ~f:(fun _kind src dst ->
          add_rule sctx
            (preprocessor_deps
             >>>
             Build.path src
             >>^ (fun _ -> [src])
             >>>
             Action.run sctx
               (Redirect
                  (Stdout,
                   target_var,
                   Chdir (root_var,
                          action)))
               ~dir
               ~dep_kind
               ~targets:(Static [dst])
               ~scope))
      | Pps { pps; flags } ->
        let ppx_exe = get_ppx_driver sctx pps ~dir ~dep_kind in
        pped_module m ~dir ~f:(fun kind src dst ->
          add_rule sctx
            (preprocessor_deps
             >>>
             Build.run ~context:sctx.context
               (Dep ppx_exe)
               [ As flags
               ; A "--dump-ast"
               ; As (cookie_library_name lib_name)
               ; A "-o"; Target dst
               ; Ml_kind.ppx_driver_flag kind; Dep src
               ])
        )
    )
end

let expand_and_eval_set t ~scope ~dir set ~standard =
  let open Build.O in
  let f = expand_vars t ~scope ~dir in
  match Ordered_set_lang.Unexpanded.files set ~f |> String_set.elements with
  | [] ->
    let set = Ordered_set_lang.Unexpanded.expand set ~files_contents:String_map.empty ~f in
    Build.return (Ordered_set_lang.eval_with_standard set ~standard)
  | files ->
    let paths = List.map files ~f:(Path.relative dir) in
    Build.all (List.map paths ~f:Build.read_sexp)
    >>^ fun sexps ->
    let files_contents = List.combine files sexps |> String_map.of_alist_exn in
    let set = Ordered_set_lang.Unexpanded.expand set ~files_contents ~f in
    Ordered_set_lang.eval_with_standard set ~standard
