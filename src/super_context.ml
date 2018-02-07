open Import
open Jbuild

module A = Action
module Pset = Path.Set
module Alias = Build_system.Alias

module Dir_with_jbuild = struct
  type t =
    { src_dir : Path.t
    ; ctx_dir : Path.t
    ; stanzas : Stanzas.t
    ; scope   : Lib_db.Scope.t With_required_by.t
    }
end

type t =
  { context                          : Context.t
  ; build_system                     : Build_system.t
  ; libs                             : Lib_db.t
  ; stanzas                          : Dir_with_jbuild.t list
  ; packages                         : Package.t String_map.t
  ; file_tree                        : File_tree.t
  ; artifacts                        : Artifacts.t
  ; stanzas_to_consider_for_install  : (Path.t * Stanza.t) list
  ; libs_vfile                       : (module Vfile_kind.S with type t = Lib.t list)
  ; cxx_flags                        : string list
  ; vars                             : Action.Var_expansion.t String_map.t
  ; ppx_dir                          : Path.t
  ; chdir                            : (Action.t, Action.t) Build.t
  ; host                             : t option
  }

let context t = t.context
let stanzas t = t.stanzas
let packages t = t.packages
let artifacts t = t.artifacts
let file_tree t = t.file_tree
let stanzas_to_consider_for_install t = t.stanzas_to_consider_for_install
let cxx_flags t = t.cxx_flags
let libs t = t.libs

let host_sctx t = Option.value t.host ~default:t

let expand_var_no_root t var = String_map.find var t.vars

let expand_vars t ~(scope : Lib_db.Scope.t) ~dir s =
  String_with_vars.expand s ~f:(fun _loc -> function
    | "ROOT" -> Some (Path.reach ~from:dir t.context.build_dir)
    | "SCOPE_ROOT" ->
      Some (Path.reach ~from:dir (Lib_db.Scope.root scope))
    | var ->
      let open Action.Var_expansion in
      expand_var_no_root t var
      |> Option.map ~f:(function
        | Paths(p,_) -> let p = List.map p ~f:Path.to_string in
          String.concat ~sep:" " p
        | Strings(s,_) -> String.concat ~sep:" " s))

let resolve_program t ?hint bin =
  Artifacts.binary ?hint t.artifacts bin

let create
      ~(context:Context.t)
      ?host
      ~scopes
      ~file_tree
      ~packages
      ~stanzas
      ~filter_out_optional_stanzas_with_missing_deps
      ~build_system
  =
  let internal_libraries =
    List.concat_map stanzas ~f:(fun (dir, _, stanzas) ->
      let ctx_dir = Path.append context.build_dir dir in
      List.filter_map stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib -> Some (ctx_dir, lib)
        | _ -> None))
  in
  let libs =
    let scopes =
      List.map scopes ~f:(fun scope ->
        { scope with Scope.root = Path.append context.build_dir scope.Scope.root })
    in
    Lib_db.create context.findlib internal_libraries
      ~scopes ~root:context.build_dir
  in
  let stanzas =
    List.map stanzas
      ~f:(fun (dir, _, stanzas) ->
        let ctx_dir = Path.append context.build_dir dir in
        { Dir_with_jbuild.
          src_dir = dir
        ; ctx_dir
        ; stanzas
        ; scope = Lib_db.find_scope' libs ~dir:ctx_dir
        })
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
          let scope = Lib_db.find_scope' libs ~dir in
          List.map (list string sexp) ~f:(Lib_db.Scope.find_exn scope)
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
    let open Action.Var_expansion in
    let open Action.Var_expansion.Concat_or_split in
    let make =
      match Bin.make with
      | None   -> Strings (["make"], Split)
      | Some p -> Paths ([p], Split)
    in
    let cflags = String.extract_blank_separated_words context.ocamlc_cflags in
    [ "-verbose"       , Strings ([] (*"-verbose";*), Concat)
    ; "CPP"            , Strings (context.c_compiler :: cflags @ ["-E"], Split)
    ; "PA_CPP"         , Strings (context.c_compiler :: cflags
                                  @ ["-undef"; "-traditional"; "-x"; "c"; "-E"],
                                  Split)
    ; "CC"             , Strings (context.c_compiler :: cflags, Split)
    ; "CXX"            , Strings (context.c_compiler :: cxx_flags, Split)
    ; "ocaml_bin"      , Paths ([context.ocaml_bin], Split)
    ; "OCAML"          , Paths ([context.ocaml], Split)
    ; "OCAMLC"         , Paths ([context.ocamlc], Split)
    ; "OCAMLOPT"       , Paths ([ocamlopt], Split)
    ; "ocaml_version"  , Strings ([context.version], Concat)
    ; "ocaml_where"    , Paths ([context.stdlib_dir], Concat)
    ; "ARCH_SIXTYFOUR" , Strings ([string_of_bool context.arch_sixtyfour],
                                  Concat)
    ; "MAKE"           , make
    ; "null"           , Paths ([Config.dev_null], Concat)
    ]
    |> String_map.of_alist
    |> function
    | Ok x -> x
    | Error _ -> assert false
  in
  { context
  ; host
  ; build_system
  ; libs
  ; stanzas
  ; packages
  ; file_tree
  ; stanzas_to_consider_for_install
  ; libs_vfile = (module Libs_vfile)
  ; artifacts
  ; cxx_flags
  ; vars
  ; ppx_dir = Path.relative context.build_dir ".ppx"
  ; chdir = Build.arr (fun (action : Action.t) ->
      match action with
      | Chdir _ -> action
      | _ -> Chdir (context.build_dir, action))
  }

let add_rule t ?sandbox ?mode ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  Build_system.add_rule t.build_system
    (Build_interpret.Rule.make ?sandbox ?mode ?locks ?loc
       ~context:t.context build)

let add_rule_get_targets t ?sandbox ?mode ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  let rule =
    Build_interpret.Rule.make ?sandbox ?mode ?locks ?loc
      ~context:t.context build
  in
  Build_system.add_rule t.build_system rule;
  List.map rule.targets ~f:Build_interpret.Target.path

let add_rules t ?sandbox builds =
  List.iter builds ~f:(add_rule t ?sandbox)

let add_alias_deps t alias deps =
  Alias.add_deps t.build_system alias deps

let add_alias_action t alias ?locks ~stamp action =
  Alias.add_action t.build_system alias ?locks ~stamp action

let eval_glob t ~dir re = Build_system.eval_glob t.build_system ~dir re
let load_dir t ~dir = Build_system.load_dir t.build_system ~dir
let on_load_dir t ~dir ~f = Build_system.on_load_dir t.build_system ~dir ~f

let source_files t ~src_path =
  match File_tree.find_dir t.file_tree src_path with
  | None -> String_set.empty
  | Some dir -> File_tree.Dir.files dir

let unique_library_name t lib =
  Lib_db.unique_library_name t.libs lib

module Libs = struct
  open Build.O
  open Lib_db

  let anonymous_scope t = Lib_db.anonymous_scope t.libs
  let external_scope t = Lib_db.external_scope t.libs

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

  let closure t ~scope ~dep_kind lib_deps =
    let internals, externals, fail =
      Lib_db.Scope.interpret_lib_deps scope lib_deps in
    with_fail ~fail
      (Build.record_lib_deps ~kind:dep_kind lib_deps
       >>>
       Build.all
         (List.map internals ~f:(fun ((dir, lib) : Lib.Internal.t) ->
            load_requires t ~dir ~item:lib.name))
       >>^ (fun internal_deps ->
         let externals =
           Findlib.closure externals
             ~required_by:scope.required_by
             ~local_public_libs:(local_public_libs t.libs)
           |> List.map ~f:(fun pkg -> Lib.External pkg)
         in
         Lib.remove_dups_preserve_order
           (List.concat (externals :: internal_deps) @
            List.map internals ~f:(fun x -> Lib.Internal x))))

  let closed_ppx_runtime_deps_of t ~scope ~dep_kind lib_deps =
    let internals, externals, fail =
      Lib_db.Scope.interpret_lib_deps scope lib_deps in
    with_fail ~fail
      (Build.record_lib_deps ~kind:dep_kind lib_deps
       >>>
       Build.all
         (List.map internals ~f:(fun ((dir, lib) : Lib.Internal.t) ->
            load_runtime_deps t ~dir ~item:lib.name))
       >>^ (fun libs ->
         let externals =
           Findlib.closed_ppx_runtime_deps_of externals
             ~required_by:scope.required_by
             ~local_public_libs:(local_public_libs t.libs)
           |> List.map ~f:(fun pkg -> Lib.External pkg)
         in
         Lib.remove_dups_preserve_order (List.concat (externals :: libs))))

  let add_select_rules t ~dir ~scope lib_deps =
    Lib_db.Scope.resolve_selects scope lib_deps
    |> List.iter ~f:(fun { dst_fn; src_fn } ->
      let src = Path.relative dir src_fn in
      let dst = Path.relative dir dst_fn in
      add_rule t
        (Build.path src
         >>>
         Build.action ~targets:[dst]
           (Copy_and_add_line_directive (src, dst))))

  let real_requires t ~dir ~scope ~dep_kind ~item ~libraries ~preprocess
        ~virtual_deps =
    let all_pps =
      List.map (Preprocess_map.pps preprocess) ~f:Pp.to_string
    in
    let vrequires = vrequires t ~dir ~item in
    add_rule t
      (Build.record_lib_deps ~kind:dep_kind (List.map virtual_deps ~f:Lib_dep.direct)
       >>>
       Build.fanout
         (closure t ~scope ~dep_kind libraries)
         (closed_ppx_runtime_deps_of t ~scope ~dep_kind
            (List.map all_pps ~f:Lib_dep.direct))
       >>>
       Build.arr (fun (libs, rt_deps) ->
         Lib.remove_dups_preserve_order (libs @ rt_deps))
       >>>
       Build.store_vfile vrequires);
    Build.vpath vrequires

  let requires t ~dir ~scope ~dep_kind ~item ~libraries ~preprocess ~virtual_deps
        ~has_dot_merlin =
    let real_requires = real_requires t ~dir ~scope ~dep_kind ~item ~libraries
                          ~preprocess ~virtual_deps
    in
    let requires =
      if t.context.merlin && has_dot_merlin then
        Build.path (Path.relative dir ".merlin-exists")
        >>>
        real_requires
      else
        real_requires
    in
    (requires, real_requires)

  let setup_runtime_deps t ~dir ~scope ~dep_kind ~item ~libraries
        ~ppx_runtime_libraries =
    let vruntime_deps = vruntime_deps t ~dir ~item in
    add_rule t
      (Build.fanout
         (closure t ~scope ~dep_kind (List.map ppx_runtime_libraries ~f:Lib_dep.direct))
         (closed_ppx_runtime_deps_of t ~scope ~dep_kind libraries)
       >>>
       Build.arr (fun (rt_deps, rt_deps_of_deps) ->
         Lib.remove_dups_preserve_order (rt_deps @ rt_deps_of_deps))
       >>>
       Build.store_vfile vruntime_deps)

  let lib_files_alias ((dir, lib) : Lib.Internal.t) ~ext =
    Alias.make (sprintf "lib-%s%s-all" lib.name ext) ~dir

  let setup_file_deps_alias t lib ~ext files =
    add_alias_deps t (lib_files_alias lib ~ext) files

  let setup_file_deps_group_alias t lib ~exts =
    setup_file_deps_alias t lib
      ~ext:(String.concat exts ~sep:"-and-")
      (List.map exts ~f:(fun ext -> Alias.stamp_file (lib_files_alias lib ~ext)))

  let file_deps t ~ext =
    Build.dyn_paths (Build.arr (fun libs ->
      List.fold_left libs ~init:[] ~f:(fun acc (lib : Lib.t) ->
        match lib with
        | External pkg ->
          Build_system.stamp_file_for_files_of t.build_system
            ~dir:(Findlib.Package.dir pkg) ~ext :: acc
        | Internal lib ->
          Alias.stamp_file (lib_files_alias lib ~ext) :: acc)))

  let static_file_deps ~ext lib =
    Alias.dep (lib_files_alias lib ~ext)
end

module Doc = struct
  let root t = Path.relative t.context.Context.build_dir "_doc"

  let dir t lib =
    let name = unique_library_name t (Lib.Internal lib) in
    Path.relative (root t) name

  let alias t ((_, lib) as ilib) =
    let doc_dir = dir t ilib in
    Alias.make (sprintf "odoc-%s%s-all" lib.name ".odoc") ~dir:doc_dir

  let deps t =
    Build.dyn_paths (Build.arr (
      List.fold_left ~init:[] ~f:(fun acc (lib : Lib.t) ->
        match lib with
        | External _ -> acc
        | Internal lib -> (Alias.stamp_file (alias t lib)) :: acc
      )))

  let static_deps t lib = Alias.dep (alias t lib)

  let setup_deps t lib files = add_alias_deps t (alias t lib) files
end

module Deps = struct
  open Build.O
  open Dep_conf

  let make_alias t ~scope ~dir s =
    Alias.of_path (Path.relative dir (expand_vars t ~scope ~dir s))

  let dep t ~scope ~dir = function
    | File  s ->
      let path = Path.relative dir (expand_vars t ~scope ~dir s) in
      Build.path path
      >>^ fun () -> [path]
    | Alias s ->
      Alias.dep (make_alias t ~scope ~dir s)
      >>^ fun () -> []
    | Alias_rec s ->
      Alias.dep_rec ~loc:(String_with_vars.loc s) ~file_tree:t.file_tree
        (make_alias t ~scope ~dir s)
      >>^ fun () -> []
    | Glob_files s -> begin
        let path = Path.relative dir (expand_vars t ~scope ~dir s) in
        let loc = String_with_vars.loc s in
        match Glob_lexer.parse_string (Path.basename path) with
        | Ok re ->
          let dir = Path.parent path in
          Build.paths_glob ~loc ~dir (Re.compile re)
        | Error (_pos, msg) ->
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

let parse_bang var : bool * string =
  let len = String.length var in
  if len > 0 && var.[0] = '!' then
    (true, String.sub var ~pos:1 ~len:(len - 1))
  else
    (false, var)

module Action = struct
  open Build.O
  module U = Action.Unexpanded

  type targets =
    | Static of Path.t list
    | Infer
    | Alias

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

  let map_exe sctx =
    match sctx.host with
    | None -> (fun exe -> exe)
    | Some host ->
      fun exe ->
        match Path.extract_build_context_dir exe with
        | Some (dir, exe) when dir = sctx.context.build_dir ->
          Path.append host.context.build_dir exe
        | _ -> exe

  let parse_lib_file ~loc s =
    match String.lsplit2 s ~on:':' with
    | None ->
      Loc.fail loc "invalid ${lib:...} form: %s" s
    | Some x -> x

  let expand_step1 sctx ~dir ~dep_kind ~scope ~targets_written_by_user ~map_exe t =
    let acc =
      { failures  = []
      ; lib_deps  = String_map.empty
      ; sdeps     = Pset.empty
      ; ddeps     = String_map.empty
      }
    in
    let t =
      U.partial_expand t ~dir ~map_exe ~f:(fun loc key ->
        let open Action.Var_expansion in
        let has_bang, var = parse_bang key in
        if has_bang then
          Loc.warn loc "The use of the variable prefix '!' is deprecated, \
                        simply use '${%s}'@." var;
        match String.lsplit2 var ~on:':' with
        | Some ("path-no-dep", s) -> Some (path_exp (Path.relative dir s))
        | Some ("exe"     , s) ->
          let exe = map_exe (Path.relative dir s) in
          static_dep_exp acc exe
        | Some ("path"    , s) -> static_dep_exp acc (Path.relative dir s)
        | Some ("bin"     , s) -> begin
            let sctx = host_sctx sctx in
            match Artifacts.binary (artifacts sctx) s with
            | Ok path ->
              static_dep_exp acc path
            | Error e ->
              add_fail acc ({ fail = fun () -> Action.Prog.Not_found.raise e })
          end
        (* "findlib" for compatibility with Jane Street packages which are not yet updated
           to convert "findlib" to "lib" *)
        | Some (("lib"|"findlib"), s) -> begin
            let lib_dep, file = parse_lib_file ~loc s in
            add_lib_dep acc lib_dep dep_kind;
            match
              Artifacts.file_of_lib (artifacts sctx) ~from:dir
                ~lib:lib_dep ~file
            with
            | Ok path -> static_dep_exp acc path
            | Error fail -> add_fail acc fail
          end
        | Some ("libexec" , s) -> begin
            let sctx = host_sctx sctx in
            let lib_dep, file = parse_lib_file ~loc s in
            add_lib_dep acc lib_dep dep_kind;
            match
              Artifacts.file_of_lib (artifacts sctx) ~from:dir
                ~lib:lib_dep ~file
            with
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
          Some (str_exp (string_of_bool (
            (* XXX should we really be using the required_by of scope here? lib
               isn't really required here, but optional *)
            Lib_db.Scope.lib_is_available scope lib)))
        | Some ("version", s) -> begin
            match Lib_db.Scope.resolve scope s with
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
              >>^ fun s -> Strings ([s], Concat)
            in
            add_ddep acc ~key data
          end
        | Some ("read-lines", s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.lines_of path
              >>^ fun l -> Strings (l, Split)
            in
            add_ddep acc ~key data
          end
        | Some ("read-strings", s) -> begin
            let path = Path.relative dir s in
            let data =
              Build.strings path
              >>^ fun l -> Strings (l, Split)
            in
            add_ddep acc ~key data
          end
        | _ ->
          match var with
          | "ROOT" -> Some (path_exp sctx.context.build_dir)
          | "SCOPE_ROOT" -> Some (path_exp (Lib_db.Scope.root scope.data))
          | "@" -> begin
              match targets_written_by_user with
              | Infer -> Loc.fail loc "You cannot use ${@} with inferred rules."
              | Alias -> Loc.fail loc "You cannot use ${@} in aliases."
              | Static l -> Some (Paths (l, Split))
            end
          | _ -> expand_var_no_root sctx var)
    in
    (t, acc)

  let expand_step2 ~dir ~dynamic_expansions ~deps_written_by_user ~map_exe t =
    let open Action.Var_expansion in
    U.Partial.expand t ~dir ~map_exe ~f:(fun loc key ->
      match String_map.find key dynamic_expansions with
      | Some _ as opt -> opt
      | None ->
        let _, var = parse_bang key in
        match var with
        | "<" ->
          Some
            (match deps_written_by_user with
             | [] ->
                Loc.warn loc "Variable '<' used with no explicit \
                              dependencies@.";
                Strings ([""], Split)
             | dep :: _ ->
               Paths ([dep], Split))
        | "^" -> Some (Paths (deps_written_by_user, Split))
        | _ -> None)

  let run sctx t ~dir ~dep_kind ~targets:targets_written_by_user ~scope
    : (Path.t list, Action.t) Build.t =
    let map_exe = map_exe sctx in
    if targets_written_by_user = Alias then begin
      match Action.Infer.unexpanded_targets t with
      | [] -> ()
      | x :: _ ->
        let loc = String_with_vars.loc x in
        Loc.warn loc "Aliases must not have targets, this target will be ignored.\n\
                      This will become an error in the future.";
    end;
    let t, forms =
      expand_step1 sctx t ~dir ~dep_kind ~scope
        ~targets_written_by_user ~map_exe
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
      | Alias ->
        let { Action.Infer.Outcome. deps; targets = _ } =
          Action.Infer.partial t ~all_targets:false
        in
        { deps; targets = Pset.empty }
    in
    let targets = Pset.elements targets in
    List.iter targets ~f:(fun target ->
      if Path.parent target <> dir then
        Loc.fail (Loc.in_file (Utils.describe_target (Utils.jbuild_file_in ~dir)))
          "A rule in this jbuild has targets in a different directory \
           than the current one, this is not allowed by Jbuilder at the moment:\n%s"
          (List.map targets ~f:(fun target ->
             sprintf "- %s" (Utils.describe_target target))
           |> String.concat ~sep:"\n"));
    let build =
      Build.record_lib_deps_simple forms.lib_deps
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
          expand_step2 t ~dir ~dynamic_expansions ~deps_written_by_user ~map_exe
        in
        Action.Unresolved.resolve unresolved ~f:(fun prog ->
          let sctx = host_sctx sctx in
          match Artifacts.binary sctx.artifacts prog with
          | Ok path    -> path
          | Error fail -> Action.Prog.Not_found.raise fail))
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

  let build_ppx_driver sctx ~scope ~dep_kind ~target pp_names ~driver =
    let ctx = sctx.context in
    let mode = Context.best_mode ctx in
    let compiler = Option.value_exn (Context.compiler ctx mode) in
    let pp_names = pp_names @ [migrate_driver_main] in
    let libs =
      Libs.closure sctx ~scope ~dep_kind (List.map pp_names ~f:Lib_dep.direct)
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
              | External pkg -> is_driver (Findlib.Package.name pkg)
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
      match Lib_db.Scope.find scope migrate_driver_main with
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
       Build.dyn_paths (Build.arr (fun libs ->
         List.rev_append
           (Lib.archive_files ~mode ~ext_lib:ctx.ext_lib libs)
           (List.filter_map libs ~f:(function
              | Lib.Internal (dir, lib) ->
                Some (Path.relative dir (lib.name ^ ctx.ext_lib))
              | External _ ->
                None))))
       >>>
       Build.run ~context:ctx (Ok compiler)
         [ A "-o" ; Target target
         ; Dyn (Lib.link_flags ~mode ~stdlib_dir:ctx.stdlib_dir)
         ])

  let gen_rules sctx components =
    match components with
    | [key] ->
      let ppx_dir = Path.relative sctx.ppx_dir key in
      let exe = Path.relative ppx_dir "ppx.exe" in
      let (key, scope) =
        match String.rsplit2 key ~on:'@' with
        | None ->
          (key, Libs.external_scope sctx)
        | Some (key, scope) ->
          (key, Lib_db.find_scope_by_name_exn sctx.libs ~name:scope) in
      let names =
        match key with
        | "+none+" -> []
        | _ -> String.split key ~on:'+'
      in
      let driver, names =
        match List.rev names with
        | [] -> (None, [])
        | driver :: rest ->
          (Some driver, List.sort rest ~cmp:String.compare @ [driver])
      in
      let scope =
        { With_required_by.
          data = scope
        ; required_by = [Preprocess names]
        } in
      build_ppx_driver sctx names ~scope ~dep_kind:Required ~target:exe ~driver
    | _ -> ()

  let get_ppx_driver sctx ~scope pps =
    let (driver, names) =
      match List.rev_map pps ~f:Pp.to_string with
      | [] -> (None, [])
      | driver :: rest -> (Some driver, rest)
    in
    let sctx = host_sctx sctx in
    let public_name name =
      match Lib_db.Scope.find scope name with
      | None -> Some name (* XXX unknown but assume it's public *)
      | Some lib -> Lib.public_name lib in
    let (driver_private, driver) =
      match driver with
      | None -> (false, None)
      | Some driver ->
        begin match public_name driver with
        | None -> (true, Some driver)
        | Some driver -> (false, Some driver)
        end in
    let (libs, has_private_libs) =
      List.fold_left ~f:(fun (libs, has_private_libs) lib ->
        match public_name lib with
        | None -> (lib :: libs, true)
        | Some pub_name -> (pub_name :: libs, has_private_libs)
      ) ~init:([], driver_private) names in
    let libs = List.sort ~cmp:String.compare libs in
    let names =
      match driver with
      | None -> libs
      | Some driver -> libs @ [driver] in
    let key =
      match names with
      | [] -> "+none+"
      | _  -> String.concat names ~sep:"+"
    in
    let sctx = host_sctx sctx in
    let ppx_dir =
      Path.relative sctx.ppx_dir (
        if has_private_libs then (
          sprintf "%s@%s" key (Lib_db.Scope.name scope.data)
        ) else (
          key
        )
      ) in
    Path.relative ppx_dir "ppx.exe"

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
    let refmt =
      Artifacts.binary sctx.artifacts "refmt" ~hint:"opam install reason" in
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

  let uses_ppx_driver ~pps =
    match Option.map ~f:Pp.to_string (List.last pps) with
    | Some "ppx_driver.runner" -> true
    | Some _ | None -> false

  let promote_correction ~uses_ppx_driver fn build =
    if not uses_ppx_driver then
      build
    else
      Build.progn
        [ build
        ; Build.return
            (A.diff ~optional:true
               fn
               (Path.extend_basename fn ~suffix:".ppx-corrected"))
        ]

  let lint_module sctx ~(source : Module.t) ~(ast : Module.t) ~dir
        ~dep_kind ~lint ~lib_name ~scope =
    let alias = Alias.lint ~dir in
    let add_alias fn build =
      Alias.add_action sctx.build_system alias build
        ~stamp:(List [ Atom "lint"
                     ; Sexp.To_sexp.(option string) lib_name
                     ; Atom fn
                     ])
    in
    match Preprocess_map.find source.name lint with
    | No_preprocessing -> ()
    | Action action ->
      let action = Action.U.Chdir (root_var, action) in
      Module.iter source ~f:(fun _ (src : Module.File.t) ->
        let src_path = Path.relative dir src.name in
        add_alias src.name
          (Build.path src_path
           >>^ (fun _ -> [src_path])
           >>> Action.run sctx
                 action
                 ~dir
                 ~dep_kind
                 ~targets:(Static [])
                 ~scope)
      )
    | Pps { pps; flags } ->
      let ppx_exe = get_ppx_driver sctx ~scope pps in
      Module.iter ast ~f:(fun kind src ->
        let src_path = Path.relative dir src.name in
        let args =
          [ Arg_spec.As flags
          ; As (cookie_library_name lib_name)
          ; Ml_kind.ppx_driver_flag kind
          ; Dep src_path
          ]
        in
        let uses_ppx_driver = uses_ppx_driver ~pps in
        let args =
          (* This hack is needed until -null is standard:
             https://github.com/ocaml-ppx/ocaml-migrate-parsetree/issues/35 *)
          if uses_ppx_driver then
            args @ [ A "-null"; A "-diff-cmd"; A "-" ]
          else
            args
        in
        add_alias src.name
          (promote_correction ~uses_ppx_driver
             (Option.value_exn (Module.file ~dir source kind))
             (Build.run ~context:sctx.context (Ok ppx_exe) args))
      )

  (* Generate rules to build the .pp files and return a new module map where all filenames
     point to the .pp files *)
  let pp_and_lint_modules sctx ~dir ~dep_kind ~modules ~lint ~preprocess
        ~preprocessor_deps ~lib_name
        ~(scope : Lib_db.Scope.t With_required_by.t) =
    let preprocessor_deps =
      Build.memoize "preprocessor deps"
        (Deps.interpret sctx ~scope:scope.data ~dir preprocessor_deps)
    in
    let lint_module = lint_module sctx ~dir ~dep_kind ~lint ~lib_name ~scope in
    String_map.map modules ~f:(fun (m : Module.t) ->
      match Preprocess_map.find m.name preprocess with
      | No_preprocessing ->
        let ast = setup_reason_rules sctx ~dir m in
        lint_module ~ast ~source:m;
        ast
      | Action action ->
        let ast =
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
          |> setup_reason_rules sctx ~dir in
        lint_module ~ast ~source:m;
        ast
      | Pps { pps; flags } ->
        let ppx_exe = get_ppx_driver sctx ~scope pps in
        let ast = setup_reason_rules sctx ~dir m in
        lint_module ~ast ~source:m;
        let uses_ppx_driver = uses_ppx_driver ~pps in
        pped_module ast ~dir ~f:(fun kind src dst ->
          add_rule sctx
            (promote_correction ~uses_ppx_driver
               (Option.value_exn (Module.file m ~dir kind))
               (preprocessor_deps
                >>>
                Build.run ~context:sctx.context
                  (Ok ppx_exe)
                  [ As flags
                  ; A "--dump-ast"
                  ; As (cookie_library_name lib_name)
                  ; As (if uses_ppx_driver then ["-diff-cmd"; "-"] else [])
                  ; A "-o"; Target dst
                  ; Ml_kind.ppx_driver_flag kind; Dep src
                  ])))
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
