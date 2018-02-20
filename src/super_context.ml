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
    ; scope   : Scope.t
    }
end

type t =
  { context                          : Context.t
  ; build_system                     : Build_system.t
  ; scopes                           : Scope.DB.t
  ; public_libs                      : Lib.DB.t
  ; installed_libs                   : Lib.DB.t
  ; stanzas                          : Dir_with_jbuild.t list
  ; packages                         : Package.t String_map.t
  ; file_tree                        : File_tree.t
  ; artifacts                        : Artifacts.t
  ; stanzas_to_consider_for_install  : (Path.t * Scope.t * Stanza.t) list
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

let host_sctx t = Option.value t.host ~default:t

let public_libs    t = t.public_libs
let installed_libs t = t.installed_libs

let find_scope_by_dir  t dir  = Scope.DB.find_by_dir  t.scopes dir
let find_scope_by_name t name = Scope.DB.find_by_name t.scopes name

let expand_var_no_root t var = String_map.find var t.vars

let expand_vars t ~scope ~dir s =
  String_with_vars.expand s ~f:(fun _loc -> function
    | "ROOT" -> Some (Path.reach ~from:dir t.context.build_dir)
    | "SCOPE_ROOT" ->
      Some (Path.reach ~from:dir (Scope.root scope))
    | var ->
      expand_var_no_root t var
      |> Option.map ~f:(fun e -> Action.Var_expansion.to_string dir e))

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
  let installed_libs = Lib.DB.create_from_findlib context.findlib in
  let internal_libs =
    List.concat_map stanzas ~f:(fun (dir, _, stanzas) ->
      let ctx_dir = Path.append context.build_dir dir in
      List.filter_map stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib -> Some (ctx_dir, lib)
        | _ -> None))
  in
  let scopes, public_libs =
    let scopes =
      List.map scopes ~f:(fun (scope : Scope_info.t) ->
        { scope with root = Path.append context.build_dir scope.root })
    in
    Scope.DB.create
      ~scopes
      ~context:context.name
      ~installed_libs
      internal_libs
  in
  let stanzas =
    List.map stanzas
      ~f:(fun (dir, scope, stanzas) ->
        let ctx_dir = Path.append context.build_dir dir in
        { Dir_with_jbuild.
          src_dir = dir
        ; ctx_dir
        ; stanzas
        ; scope = Scope.DB.find_by_name scopes scope.Scope_info.name
        })
  in
  let stanzas_to_consider_for_install =
    if filter_out_optional_stanzas_with_missing_deps then
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; scope; _ } ->
        List.filter_map stanzas ~f:(fun stanza ->
          let keep =
            match (stanza : Stanza.t) with
            | Library lib -> Lib.DB.available (Scope.libs scope) lib.name
            | Install _   -> true
            | _           -> false
          in
          Option.some_if keep (ctx_dir, scope, stanza)))
    else
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; scope; _ } ->
        List.map stanzas ~f:(fun s -> (ctx_dir, scope, s)))
  in
  let artifacts =
    Artifacts.create context ~public_libs stanzas
      ~f:(fun (d : Dir_with_jbuild.t) -> d.stanzas)
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
    ; "ocaml_version"  , Strings ([context.version_string], Concat)
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
  ; scopes
  ; public_libs
  ; installed_libs
  ; stanzas
  ; packages
  ; file_tree
  ; stanzas_to_consider_for_install
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

module Libs = struct
  open Build.O

  let requires_to_build requires ~required_by =
    match requires with
    | Ok x -> Build.return x
    | Error e ->
      Build.fail
        { fail = fun () ->
            raise (Lib.Error (With_required_by.append e required_by))
        }

  let requires_generic
        t
        ~loc
        ~dir
        ~requires
        ~libraries
        ~dep_kind
        ~has_dot_merlin
    =
    let requires =
      requires_to_build requires ~required_by:[Loc loc]
    in
    let requires =
      Build.record_lib_deps ~kind:dep_kind libraries
      >>> requires
    in
    let requires_with_merlin =
      if t.context.merlin && has_dot_merlin then
        Build.path (Path.relative dir ".merlin-exists")
        >>>
        requires
      else
        requires
    in
    (requires_with_merlin, requires)

  let add_select_rules t ~dir resolved_selects =
    List.iter resolved_selects ~f:(fun rs ->
      let { Lib.Compile.Resolved_select.dst_fn; src_fn } = rs in
      let dst = Path.relative dir dst_fn in
      add_rule t
        (match src_fn with
         | Ok src_fn ->
           let src = Path.relative dir src_fn in
           Build.copy_and_add_line_directive ~src ~dst
         | Error e ->
           Build.fail ~targets:[dst]
             { fail = fun () ->
                 raise (Lib.Error { data        = No_solution_found_for_select e
                                  ; required_by = []
                                  })
             }))

  let requires_for_library t ~dir ~scope ~dep_kind (conf : Jbuild.Library.t) =
    match Lib.DB.find (Scope.libs scope) conf.name with
    | Error Not_found -> assert false
    | Error (Hidden _ as reason) ->
      let build =
        Build.fail { fail = fun () ->
          Lib.not_available ~loc:conf.buildable.loc reason "Library %S"
            conf.name  }
      in
      (build, build)
    | Ok lib ->
      add_select_rules t ~dir (Lib.Compile.resolved_selects lib);
      let libraries =
        List.fold_left conf.virtual_deps ~init:conf.buildable.libraries
          ~f:(fun acc s -> Lib_dep.Direct s :: acc)
      in
      requires_generic t ~dir ~loc:conf.buildable.loc
        ~requires:(Lib.Compile.requires lib)
        ~libraries
        ~dep_kind
        ~has_dot_merlin:conf.buildable.gen_dot_merlin

  let requires t ~loc ~dir ~scope ~dep_kind ~libraries
        ~preprocess ~has_dot_merlin =
    let requires, resolved_selects =
      Lib.DB.resolve_user_written_deps (Scope.libs scope)
        libraries
        ~pps:(Jbuild.Preprocess_map.pps preprocess)
    in
    add_select_rules t ~dir resolved_selects;
    requires_generic t ~dir ~loc
      ~requires
      ~libraries
      ~dep_kind
      ~has_dot_merlin

  let lib_files_alias ~dir ~name ~ext =
    Alias.make (sprintf "lib-%s%s-all" name ext) ~dir

  let setup_file_deps_alias t ~dir ~ext lib files =
    add_alias_deps t
      (lib_files_alias ~dir ~name:(Library.best_name lib) ~ext) files

  let setup_file_deps_group_alias t ~dir ~exts lib =
    setup_file_deps_alias t lib ~dir
      ~ext:(String.concat exts ~sep:"-and-")
      (List.map exts ~f:(fun ext ->
         Alias.stamp_file
           (lib_files_alias ~dir ~name:(Library.best_name lib) ~ext)))

  let file_deps t ~ext =
    Build.dyn_paths (Build.arr (fun libs ->
      List.fold_left libs ~init:[] ~f:(fun acc (lib : Lib.t) ->
        let x =
          if Lib.is_local lib then
            Alias.stamp_file
              (lib_files_alias ~dir:(Lib.src_dir lib) ~name:(Lib.name lib) ~ext)
          else
            Build_system.stamp_file_for_files_of t.build_system
              ~dir:(Lib.obj_dir lib) ~ext
        in
        x :: acc)))
end

module Doc = struct
  let root t = Path.relative t.context.Context.build_dir "_doc"

  type origin =
    | Public  of string
    | Private of string * Scope_info.Name.t

  let dir_internal t origin =
    let name =
      match origin with
      | Public   n     -> n
      | Private (n, s) -> sprintf "%s@%s" n (Scope_info.Name.to_string s)
    in
    Path.relative (root t) name

  let dir t (lib : Library.t) =
    dir_internal t
      (match lib.public with
       | Some { name; _ } -> Public name
       | None             -> Private (lib.name, lib.scope_name))

  let alias = Alias.make ".doc-all"

  let deps t =
    Build.dyn_paths (Build.arr (
      List.fold_left ~init:[] ~f:(fun acc (lib : Lib.t) ->
        if Lib.is_local lib then (
          let dir =
            dir_internal t
              (match Lib.status lib with
               | Installed -> assert false
               | Public    -> Public (Lib.name lib)
               | Private s -> Private (Lib.name lib, s))
          in
          Alias.stamp_file (alias ~dir) :: acc
        ) else (
          acc
        )
      )))

  let alias t lib = alias ~dir:(dir t lib)

  let static_deps t lib = Alias.dep (alias t lib)

  let setup_deps t lib files = add_alias_deps t (alias t lib) files

  let dir t lib = dir t lib
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
        let t = C.option C.atom
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
              Artifacts.file_of_lib (artifacts sctx) ~loc ~lib:lib_dep ~file
            with
            | Ok path -> static_dep_exp acc path
            | Error fail -> add_fail acc fail
          end
        | Some ("libexec" , s) -> begin
            let sctx = host_sctx sctx in
            let lib_dep, file = parse_lib_file ~loc s in
            add_lib_dep acc lib_dep dep_kind;
            match
              Artifacts.file_of_lib (artifacts sctx) ~loc ~lib:lib_dep ~file
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
            Lib.DB.available (Scope.libs scope) lib)))
        | Some ("version", s) -> begin
            match Scope_info.resolve (Scope.info scope) s with
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
          | "SCOPE_ROOT" -> Some (path_exp (Scope.root scope))
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
                Strings ([""], Concat)
             | dep :: _ ->
               Paths ([dep], Concat))
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
    let pped_file (kind : Ml_kind.t) (file : Module.File.t) =
      let pp_fname = pp_fname file.name in
      f kind (Path.relative dir file.name) (Path.relative dir pp_fname);
      {file with name = pp_fname}
    in
    { m with
      impl = Option.map m.impl ~f:(pped_file Impl)
    ; intf = Option.map m.intf ~f:(pped_file Intf)
    }

  let migrate_driver_main = "ocaml-migrate-parsetree.driver-main"

  let build_ppx_driver sctx ~lib_db ~dep_kind ~target pps =
    let ctx = sctx.context in
    let mode = Context.best_mode ctx in
    let compiler = Option.value_exn (Context.compiler ctx mode) in
    let pps = pps @ [Pp.of_string migrate_driver_main] in
    let driver, libs =
      let resolved_pps = Lib.DB.resolve_pps lib_db pps in
      let driver =
        match resolved_pps with
        | Ok    l -> List.last l
        | Error _ -> None
      in
      (driver,
       Result.bind resolved_pps ~f:Lib.closure
       |> Libs.requires_to_build
            ~required_by:[Preprocess (pps : Jbuild.Pp.t list :> string list)])
    in
    let libs =
      Build.record_lib_deps ~kind:dep_kind (List.map pps ~f:Lib_dep.of_pp)
      >>>
      libs
    in
    let libs =
      (* Put the driver back at the end, just before migrate_driver_main *)
      match driver with
      | None -> libs
      | Some driver ->
        libs >>^ fun libs ->
        let libs, drivers =
          List.partition_map libs ~f:(fun lib ->
            if lib == driver || Lib.name lib = migrate_driver_main then
              Inr lib
            else
              Inl lib)
        in
        let user_driver, migrate_driver =
          List.partition_map drivers ~f:(fun lib ->
            if Lib.name lib = migrate_driver_main then
              Inr lib
            else
              Inl lib)
        in
        libs @ user_driver @ migrate_driver
    in
    (* Provide a better error for migrate_driver_main given that this is an implicit
       dependency *)
    let libs =
      match Lib.DB.available lib_db migrate_driver_main with
      | false ->
        Build.fail { fail = fun () ->
          die "@{<error>Error@}: I couldn't find '%s'.\n\
               I need this library in order to use ppx rewriters.\n\
               See the manual for details.\n\
               Hint: opam install ocaml-migrate-parsetree"
            migrate_driver_main
        }
        >>>
        libs
      | true ->
        libs
    in
    add_rule sctx
      (libs
       >>>
       Build.dyn_paths
         (Build.arr
            (Lib.L.archive_files ~mode ~ext_lib:ctx.ext_lib))
       >>>
       Build.run ~context:ctx (Ok compiler)
         [ A "-o" ; Target target
         ; Dyn (Lib.L.link_flags ~mode ~stdlib_dir:ctx.stdlib_dir)
         ])

  let gen_rules sctx components =
    match components with
    | [key] ->
      let ppx_dir = Path.relative sctx.ppx_dir key in
      let exe = Path.relative ppx_dir "ppx.exe" in
      let (key, lib_db) =
        match String.rsplit2 key ~on:'@' with
        | None ->
          (key, sctx.public_libs)
        | Some (key, scope) ->
          (key, Scope.libs (find_scope_by_name sctx
                              (Scope_info.Name.of_string scope)))
      in
      let names =
        match key with
        | "+none+" -> []
        | _ -> String.split key ~on:'+'
      in
      let names =
        match List.rev names with
        | [] -> []
        | driver :: rest -> List.sort rest ~cmp:String.compare @ [driver]
      in
      let pps = List.map names ~f:Jbuild.Pp.of_string in
      build_ppx_driver sctx pps ~lib_db ~dep_kind:Required ~target:exe
    | _ -> ()

  let most_specific_db (a : Lib.Status.t) (b : Lib.Status.t) =
    match a, b with
    | Private x, Private y -> assert (x = y); a
    | Private _, _         -> a
    | _        , Private _ -> b
    | Public   , _
    | _        , Public    -> Public
    | Installed, Installed -> Installed

  let get_ppx_driver sctx ~scope pps =
    let driver, names =
      match List.rev_map pps ~f:Pp.to_string with
      | [] -> (None, [])
      | driver :: rest -> (Some driver, rest)
    in
    let sctx = host_sctx sctx in
    let name_and_db name =
      match Lib.DB.find (Scope.libs scope) name with
      | Error _ ->
        (* XXX unknown but assume it's public *)
        (name, Lib.Status.Installed)
      | Ok lib ->
        (Lib.name lib, Lib.status lib)
    in
    let driver, driver_db =
      match driver with
      | None -> (None, Lib.Status.Installed)
      | Some driver ->
        let name, db = name_and_db driver in
        (Some name, db)
    in
    let names, db =
      List.fold_left names ~init:([], driver_db) ~f:(fun (names, db) lib ->
        let name, db' = name_and_db lib in
        (name :: names, most_specific_db db db'))
    in
    let names = List.sort ~cmp:String.compare names in
    let names =
      match driver with
      | None        -> names
      | Some driver -> names @ [driver]
    in
    let key =
      match names with
      | [] -> "+none+"
      | _  -> String.concat names ~sep:"+"
    in
    let key =
      match db with
      | Installed | Public -> key
      | Private scope_name ->
        sprintf "%s@%s" key (Scope_info.Name.to_string scope_name)
    in
    let sctx = host_sctx sctx in
    let ppx_dir = Path.relative sctx.ppx_dir key in
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
    let to_ml (f : Module.File.t) =
      match f.syntax with
      | OCaml  -> f
      | Reason ->
        let ml = Module.File.to_ocaml f in
        add_rule sctx (rule f.name ml.name);
        ml
    in
    { m with
      impl = Option.map m.impl ~f:to_ml
    ; intf = Option.map m.intf ~f:to_ml
    }

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

  let lint_module sctx ~dir ~dep_kind ~lint ~lib_name ~scope = Staged.stage (
    let alias = Alias.lint ~dir in
    let add_alias fn build =
      Alias.add_action sctx.build_system alias build
        ~stamp:(List [ Atom "lint"
                     ; Sexp.To_sexp.(option atom) lib_name
                     ; Atom fn
                     ])
    in
    let lint =
      Per_module.map lint ~f:(function
        | Preprocess.No_preprocessing ->
          (fun ~source:_ ~ast:_ -> ())
        | Action action ->
          (fun ~source ~ast:_ ->
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
                        ~scope)))
        | Pps { pps; flags } ->
          let ppx_exe = get_ppx_driver sctx ~scope pps in
          let uses_ppx_driver = uses_ppx_driver ~pps in
          let args : _ Arg_spec.t =
            S [ As flags
              ; As (cookie_library_name lib_name)
              (* This hack is needed until -null is standard:
                 https://github.com/ocaml-ppx/ocaml-migrate-parsetree/issues/35
              *)
              ; As (if uses_ppx_driver then
                      [ "-null"; "-diff-cmd"; "-" ]
                    else
                      [])
              ]
          in
          (fun ~source ~ast ->
             Module.iter ast ~f:(fun kind src ->
               let args =
                 [ args
                 ; Ml_kind.ppx_driver_flag kind
                 ; Dep (Path.relative dir src.name)
                 ]
               in
               add_alias src.name
                 (promote_correction ~uses_ppx_driver
                    (Option.value_exn (Module.file ~dir source kind))
                    (Build.run ~context:sctx.context (Ok ppx_exe) args))
             )))
    in
    fun ~(source : Module.t) ~ast ->
      Per_module.get lint source.name ~source ~ast)

  (* Generate rules to build the .pp files and return a new module map
     where all filenames point to the .pp files *)
  let pp_and_lint_modules sctx ~dir ~dep_kind ~modules ~lint ~preprocess
        ~preprocessor_deps ~lib_name ~scope =
    let preprocessor_deps =
      Build.memoize "preprocessor deps" preprocessor_deps
    in
    let lint_module =
      Staged.unstage (lint_module sctx ~dir ~dep_kind ~lint ~lib_name ~scope)
    in
    let preprocess =
      Per_module.map preprocess ~f:(function
        | Preprocess.No_preprocessing ->
          (fun m ->
             let ast = setup_reason_rules sctx ~dir m in
             lint_module ~ast ~source:m;
             ast)
        | Action action ->
          (fun m ->
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
             ast)
        | Pps { pps; flags } ->
          let ppx_exe = get_ppx_driver sctx ~scope pps in
          let uses_ppx_driver = uses_ppx_driver ~pps in
          let args : _ Arg_spec.t =
            S [ As flags
              ; A "--dump-ast"
              ; As (cookie_library_name lib_name)
              ; As (if uses_ppx_driver then ["-diff-cmd"; "-"] else [])
              ]
          in
          (fun m ->
             let ast = setup_reason_rules sctx ~dir m in
             lint_module ~ast ~source:m;
             pped_module ast ~dir ~f:(fun kind src dst ->
               add_rule sctx
                 (promote_correction ~uses_ppx_driver
                    (Option.value_exn (Module.file m ~dir kind))
                    (preprocessor_deps
                     >>>
                     Build.run ~context:sctx.context
                       (Ok ppx_exe)
                       [ args
                       ; A "-o"; Target dst
                       ; Ml_kind.ppx_driver_flag kind; Dep src
                       ])))))
    in
    String_map.map modules ~f:(fun (m : Module.t) ->
      Per_module.get preprocess m.name m)
end

module Eval_strings = Ordered_set_lang.Make(struct
    type t = string
    let name t = t
  end)

let expand_and_eval_set t ~scope ~dir set ~standard =
  let open Build.O in
  let f = expand_vars t ~scope ~dir in
  let parse ~loc:_ s = s in
  match Ordered_set_lang.Unexpanded.files set ~f |> String_set.elements with
  | [] ->
    let set = Ordered_set_lang.Unexpanded.expand set ~files_contents:String_map.empty ~f in
    Build.return (Eval_strings.eval set ~standard ~parse)
  | files ->
    let paths = List.map files ~f:(Path.relative dir) in
    Build.all (List.map paths ~f:Build.read_sexp)
    >>^ fun sexps ->
    let files_contents = List.combine files sexps |> String_map.of_alist_exn in
    let set = Ordered_set_lang.Unexpanded.expand set ~files_contents ~f in
    Eval_strings.eval set ~standard ~parse
