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
  ; packages                         : Package.t Package.Name.Map.t
  ; file_tree                        : File_tree.t
  ; artifacts                        : Artifacts.t
  ; stanzas_to_consider_for_install  : (Path.t * Scope.t * Stanza.t) list
  ; cxx_flags                        : string list
  ; vars                             : Action.Var_expansion.t String_map.t
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
let build_dir t = t.context.build_dir

let host t = Option.value t.host ~default:t

let public_libs    t = t.public_libs
let installed_libs t = t.installed_libs

let find_scope_by_dir  t dir  = Scope.DB.find_by_dir  t.scopes dir
let find_scope_by_name t name = Scope.DB.find_by_name t.scopes name

let expand_var_no_root t var = String_map.find t.vars var

let expand_vars t ~scope ~dir ?(extra_vars=String_map.empty) s =
  String_with_vars.expand s ~f:(fun _loc -> function
    | "ROOT" -> Some (Path.reach ~from:dir t.context.build_dir)
    | "SCOPE_ROOT" ->
      Some (Path.reach ~from:dir (Scope.root scope))
    | var ->
      Option.map ~f:(fun e -> Action.Var_expansion.to_string dir e)
        (match expand_var_no_root t var with
         | Some _ as x -> x
         | None -> String_map.find extra_vars var))

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
    ; "ocaml_where"    , Strings ([Path.to_string context.stdlib_dir], Concat)
    ; "ARCH_SIXTYFOUR" , Strings ([string_of_bool context.arch_sixtyfour],
                                  Concat)
    ; "MAKE"           , make
    ; "null"           , Strings ([Path.to_string Config.dev_null], Concat)
    ]
    |> String_map.of_list
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
  ; chdir = Build.arr (fun (action : Action.t) ->
      match action with
      | Chdir _ -> action
      | _ -> Chdir (context.build_dir, action))
  }

let prefix_rules t prefix ~f =
  Build_system.prefix_rules t.build_system prefix ~f

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
                 raise (Lib.Error (No_solution_found_for_select e))
             }))

  let requires t ~dir ~has_dot_merlin compile_info =
    add_select_rules t ~dir (Lib.Compile.resolved_selects compile_info);
    let requires = Build.of_result (Lib.Compile.requires compile_info) in
    let requires =
      Build.record_lib_deps (Lib.Compile.user_written_deps compile_info)
        ~kind:(if Lib.Compile.optional compile_info then
                 Optional
               else
                 Required)
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
      >>^ Pset.to_list

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
        (sprintf "%s.version.sexp" (p.name :> string))
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
    acc.lib_deps <- String_map.add acc.lib_deps lib kind

  let add_fail acc fail =
    acc.failures <- fail :: acc.failures;
    None

  let add_ddep acc ~key dep =
    acc.ddeps <- String_map.add acc.ddeps key dep;
    None

  let path_exp path = Action.Var_expansion.Paths   ([path], Concat)
  let str_exp  path = Action.Var_expansion.Strings ([path], Concat)

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

  let expand_step1 sctx ~dir ~dep_kind ~scope ~targets_written_by_user
        ~map_exe ~extra_vars t =
    let acc =
      { failures  = []
      ; lib_deps  = String_map.empty
      ; sdeps     = Pset.empty
      ; ddeps     = String_map.empty
      }
    in
    let open Action.Var_expansion in
    let expand loc key var = function
      | Some ("exe"     , s) -> Some (path_exp (map_exe (Path.relative dir s)))
      | Some ("path"    , s) -> Some (path_exp          (Path.relative dir s) )
      | Some ("bin"     , s) -> begin
          let sctx = host sctx in
          match Artifacts.binary (artifacts sctx) s with
          | Ok path -> Some (path_exp path)
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
          | Ok path -> Some (path_exp path)
          | Error fail -> add_fail acc fail
        end
      | Some ("libexec" , s) -> begin
          let sctx = host sctx in
          let lib_dep, file = parse_lib_file ~loc s in
          add_lib_dep acc lib_dep dep_kind;
          match
            Artifacts.file_of_lib (artifacts sctx) ~loc ~lib:lib_dep ~file
          with
          | Error fail -> add_fail acc fail
          | Ok path ->
            if not Sys.win32 || Filename.extension s = ".exe" then begin
              Some (path_exp path)
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
          match Scope_info.resolve (Scope.info scope)
                  (Package.Name.of_string s) with
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
        match expand_var_no_root sctx var with
        | Some _ as x -> x
        | None -> String_map.find extra_vars var
    in
    let t =
      U.partial_expand t ~dir ~map_exe ~f:(fun loc key ->
        let has_bang, var = parse_bang key in
        if has_bang then
          Loc.warn loc "The use of the variable prefix '!' is deprecated, \
                        simply use '${%s}'@." var;
        match var with
        | "ROOT" -> Some (path_exp sctx.context.build_dir)
        | "SCOPE_ROOT" -> Some (path_exp (Scope.root scope))
        | "@" -> begin
            match targets_written_by_user with
            | Infer -> Loc.fail loc "You cannot use ${@} with inferred rules."
            | Alias -> Loc.fail loc "You cannot use ${@} in aliases."
            | Static l -> Some (Paths (l, Split))
          end
        | _ ->
          match String.lsplit2 var ~on:':' with
          | Some ("path-no-dep", s) ->
            Some (path_exp (Path.relative dir s))
          | x ->
            let exp = expand loc key var x in
            (match exp with
             | Some (Paths (ps, _)) ->
               acc.sdeps <- Pset.union (Pset.of_list ps) acc.sdeps
             | _ -> ());
            exp)
    in
    (t, acc)

  let expand_step2 ~dir ~dynamic_expansions ~deps_written_by_user ~map_exe t =
    let open Action.Var_expansion in
    U.Partial.expand t ~dir ~map_exe ~f:(fun loc key ->
      match String_map.find dynamic_expansions key with
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

  let run sctx ?(extra_vars=String_map.empty)
        t ~dir ~dep_kind ~targets:targets_written_by_user ~scope
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
        ~targets_written_by_user ~map_exe ~extra_vars
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

           It's likely that what we get here is what the user thinks
           of as temporary files, even though they might conflict with
           actual targets. We need to tell jbuilder about such things,
           so that it can report better errors.

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
    let targets = Pset.to_list targets in
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
      let ddeps = String_map.to_list forms.ddeps in
      Build.first (Build.all (List.map ddeps ~f:snd))
      >>^ (fun (vals, deps_written_by_user) ->
        let dynamic_expansions =
          List.fold_left2 ddeps vals ~init:String_map.empty
            ~f:(fun acc (var, _) value -> String_map.add acc var value)
        in
        let unresolved =
          expand_step2 t ~dir ~dynamic_expansions ~deps_written_by_user ~map_exe
        in
        Action.Unresolved.resolve unresolved ~f:(fun prog ->
          let sctx = host sctx in
          match Artifacts.binary sctx.artifacts prog with
          | Ok path    -> path
          | Error fail -> Action.Prog.Not_found.raise fail))
      >>>
      Build.dyn_paths (Build.arr (fun action ->
        let { Action.Infer.Outcome.deps; targets = _ } =
          Action.Infer.infer action
        in
        Pset.to_list deps))
      >>>
      Build.action_dyn () ~dir ~targets
    in
    match forms.failures with
    | [] -> build
    | fail :: _ -> Build.fail fail >>> build
end

module Eval_strings = Ordered_set_lang.Make(struct
    type t = string
    let name t = t
  end)

let expand_and_eval_set t ~scope ~dir ?extra_vars set ~standard =
  let open Build.O in
  let f = expand_vars t ~scope ~dir ?extra_vars in
  let parse ~loc:_ s = s in
  match Ordered_set_lang.Unexpanded.files set ~f |> String_set.to_list with
  | [] ->
    let set =
      Ordered_set_lang.Unexpanded.expand set ~files_contents:String_map.empty ~f
    in
    Build.return (Eval_strings.eval set ~standard ~parse)
  | files ->
    let paths = List.map files ~f:(Path.relative dir) in
    Build.all (List.map paths ~f:Build.read_sexp)
    >>^ fun sexps ->
    let files_contents = List.combine files sexps |> String_map.of_list_exn in
    let set = Ordered_set_lang.Unexpanded.expand set ~files_contents ~f in
    Eval_strings.eval set ~standard ~parse
