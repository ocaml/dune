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

module Env_node = struct
  type t =
    { dir                 : Path.t
    ; inherit_from        : t Lazy.t option
    ; scope               : Scope.t
    ; config              : Env.t
    ; mutable ocaml_flags : Ocaml_flags.t option
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
  ; vars                             : Action.Var_expansion.t String.Map.t
  ; chdir                            : (Action.t, Action.t) Build.t
  ; host                             : t option
  ; libs_by_package : (Package.t * Lib.Set.t) Package.Name.Map.t
  ; env                              : (Path.t, Env_node.t) Hashtbl.t
  }

let context t = t.context
let stanzas t = t.stanzas
let packages t = t.packages
let libs_by_package t = t.libs_by_package
let artifacts t = t.artifacts
let file_tree t = t.file_tree
let stanzas_to_consider_for_install t = t.stanzas_to_consider_for_install
let cxx_flags t = t.cxx_flags
let build_dir t = t.context.build_dir
let profile t = t.context.profile
let build_system t = t.build_system

let host t = Option.value t.host ~default:t

let internal_lib_names t =
  List.fold_left t.stanzas ~init:String.Set.empty
    ~f:(fun acc { Dir_with_jbuild. stanzas; _ } ->
      List.fold_left stanzas ~init:acc ~f:(fun acc -> function
        | Stanza.Library lib ->
          String.Set.add
            (match lib.public with
             | None -> acc
             | Some { name; _ } -> String.Set.add acc name)
            lib.name
        | _ -> acc))

let public_libs    t = t.public_libs
let installed_libs t = t.installed_libs

let find_scope_by_dir  t dir  = Scope.DB.find_by_dir  t.scopes dir
let find_scope_by_name t name = Scope.DB.find_by_name t.scopes name

let expand_var_no_root t var = String.Map.find t.vars var

let expand_vars t ~scope ~dir ?(extra_vars=String.Map.empty) s =
  String_with_vars.expand s ~f:(fun _loc -> function
    | "ROOT" -> Some (Path.reach ~from:dir t.context.build_dir)
    | "SCOPE_ROOT" ->
      Some (Path.reach ~from:dir (Scope.root scope))
    | var ->
      Option.map ~f:(fun e -> Action.Var_expansion.to_string dir e)
        (match expand_var_no_root t var with
         | Some _ as x -> x
         | None -> String.Map.find extra_vars var))

let expand_and_eval_set t ~scope ~dir ?extra_vars set ~standard =
  let open Build.O in
  let f = expand_vars t ~scope ~dir ?extra_vars in
  let parse ~loc:_ s = s in
  match Ordered_set_lang.Unexpanded.files set ~f |> String.Set.to_list with
  | [] ->
    let set =
      Ordered_set_lang.Unexpanded.expand set ~files_contents:String.Map.empty ~f
    in
    standard >>^ fun standard ->
    Ordered_set_lang.String.eval set ~standard ~parse
  | files ->
    let paths = List.map files ~f:(Path.relative dir) in
    Build.fanout standard (Build.all (List.map paths ~f:Build.read_sexp))
    >>^ fun (standard, sexps) ->
    let files_contents = List.combine files sexps |> String.Map.of_list_exn in
    let set = Ordered_set_lang.Unexpanded.expand set ~files_contents ~f in
    Ordered_set_lang.String.eval set ~standard ~parse

module Env = struct
  open Env_node

  let rec get t ~dir =
    match Hashtbl.find t.env dir with
    | None ->
      begin match Path.parent dir with
      | None -> raise_notrace Exit
      | Some parent ->
        let node = get t ~dir:parent in
        Hashtbl.add t.env dir node;
        node
      end
    | Some node -> node

  let get t ~dir =
    match get t ~dir with
    | node -> node
    | exception Exit ->
      Exn.code_error "Super_context.Env.get called on invalid directory"
        [ "dir", Path.sexp_of_t dir ]

  let ocaml_flags t ~dir =
    let rec loop t node =
      match node.ocaml_flags with
      | Some x -> x
      | None ->
        let default =
          match node.inherit_from with
          | None -> Ocaml_flags.default ~profile:(profile t)
          | Some (lazy node) -> loop t node
        in
        let flags =
          match List.find_map node.config.rules ~f:(fun (pat, cfg) ->
            match (pat : Env.pattern), profile t with
            | Any, _ -> Some cfg
            | Profile a, b -> Option.some_if (a = b) cfg)
          with
          | None -> default
          | Some cfg ->
            Ocaml_flags.make
              ~flags:cfg.flags
              ~ocamlc_flags:cfg.ocamlc_flags
              ~ocamlopt_flags:cfg.ocamlopt_flags
              ~default
              ~eval:(expand_and_eval_set t ~scope:node.scope ~dir:node.dir
                       ?extra_vars:None)
        in
        node.ocaml_flags <- Some flags;
        flags
    in
    loop t (get t ~dir)

end

let ocaml_flags t ~dir ~scope (x : Buildable.t) =
  Ocaml_flags.make
    ~flags:x.flags
    ~ocamlc_flags:x.ocamlc_flags
    ~ocamlopt_flags:x.ocamlopt_flags
    ~default:(Env.ocaml_flags t ~dir)
    ~eval:(expand_and_eval_set t ~scope ~dir ?extra_vars:None)

let dump_env t ~dir =
  Ocaml_flags.dump (Env.ocaml_flags t ~dir)

let resolve_program t ?hint bin =
  Artifacts.binary ?hint t.artifacts bin

let create
      ~(context:Context.t)
      ?host
      ~projects
      ~file_tree
      ~packages
      ~stanzas
      ~external_lib_deps_mode
      ~build_system
  =
  let installed_libs =
    Lib.DB.create_from_findlib context.findlib ~external_lib_deps_mode
  in
  let internal_libs =
    List.concat_map stanzas ~f:(fun (dir, _, stanzas) ->
      let ctx_dir = Path.append context.build_dir dir in
      List.filter_map stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib -> Some (ctx_dir, lib)
        | _ -> None))
  in
  let scopes, public_libs =
    let projects =
      List.map projects ~f:(fun (project : Dune_project.t) ->
        { project with root = Path.append context.build_dir project.root })
    in
    Scope.DB.create
      ~projects
      ~context:context.name
      ~installed_libs
      internal_libs
  in
  let stanzas =
    List.map stanzas
      ~f:(fun (dir, project, stanzas) ->
        let ctx_dir = Path.append context.build_dir dir in
        { Dir_with_jbuild.
          src_dir = dir
        ; ctx_dir
        ; stanzas
        ; scope = Scope.DB.find_by_name scopes project.Dune_project.name
        })
  in
  let stanzas_to_consider_for_install =
    if not external_lib_deps_mode then
      List.concat_map stanzas ~f:(fun { ctx_dir; stanzas; scope; _ } ->
        List.filter_map stanzas ~f:(fun stanza ->
          let keep =
            match (stanza : Stanza.t) with
            | Library lib -> Lib.DB.available (Scope.libs scope) lib.name
            | Documentation _
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
    List.filter context.ocamlc_cflags
      ~f:(fun s -> not (String.is_prefix s ~prefix:"-std="))
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
    let cflags = context.ocamlc_cflags in
    let strings l = Strings (l  , Split)  in
    let string  s = Strings ([s], Concat) in
    let path    p = Paths   ([p], Split)  in
    let vars =
      [ "-verbose"       , Strings ([] (*"-verbose";*), Concat)
      ; "CPP"            , strings (context.c_compiler :: cflags @ ["-E"])
      ; "PA_CPP"         , strings (context.c_compiler :: cflags
                                    @ ["-undef"; "-traditional";
                                       "-x"; "c"; "-E"])
      ; "CC"             , strings (context.c_compiler :: cflags)
      ; "CXX"            , strings (context.c_compiler :: cxx_flags)
      ; "ocaml_bin"      , path context.ocaml_bin
      ; "OCAML"          , path context.ocaml
      ; "OCAMLC"         , path context.ocamlc
      ; "OCAMLOPT"       , path ocamlopt
      ; "ocaml_version"  , string context.version_string
      ; "ocaml_where"    , string (Path.to_string context.stdlib_dir)
      ; "ARCH_SIXTYFOUR" , string (string_of_bool context.arch_sixtyfour)
      ; "MAKE"           , make
      ; "null"           , string (Path.to_string Config.dev_null)
      ; "ext_obj"        , string context.ext_obj
      ; "ext_asm"        , string context.ext_asm
      ; "ext_lib"        , string context.ext_lib
      ; "ext_dll"        , string context.ext_dll
      ; "ext_exe"        , string context.ext_exe
      ]
    in
    let vars =
      vars @
      List.map (Ocaml_config.to_list context.ocaml_config) ~f:(fun (k, v) ->
        ("ocaml-config:" ^ k,
         match (v : Ocaml_config.Value.t) with
         | Bool   x -> string (string_of_bool x)
         | Int    x -> string (string_of_int x)
         | String x -> string x
         | Words  x -> strings x
         | Prog_and_args x -> strings (x.prog :: x.args)))
    in
    match String.Map.of_list vars with
    | Ok    x -> x
    | Error _ -> assert false
  in
  let t =
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
    ; libs_by_package =
        Lib.DB.all public_libs
        |> Lib.Set.to_list
        |> List.map ~f:(fun lib ->
          (Option.value_exn (Lib.package lib), lib))
        |> Package.Name.Map.of_list_multi
        |> Package.Name.Map.merge packages ~f:(fun _name pkg libs ->
          let pkg  = Option.value_exn pkg          in
          let libs = Option.value libs ~default:[] in
          Some (pkg, Lib.Set.of_list libs))
    ; env = Hashtbl.create 128
    }
  in
  List.iter stanzas
    ~f:(fun { Dir_with_jbuild. ctx_dir; scope; stanzas; _ } ->
      List.iter stanzas ~f:(function
        | Stanza.Env config ->
          let inherit_from =
            if ctx_dir = Scope.root scope then
              None
            else
              Some (lazy (Env.get t ~dir:(Path.parent_exn ctx_dir)))
          in
          Hashtbl.add t.env ctx_dir
            { dir          = ctx_dir
            ; inherit_from = inherit_from
            ; scope        = scope
            ; config       = config
            ; ocaml_flags  = None
            }
        | _ -> ()));
  if not (Hashtbl.mem t.env context.build_dir) then
    Hashtbl.add t.env context.build_dir
      { Env_node.
        dir          = context.build_dir
      ; inherit_from = None
      ; scope        = Scope.DB.find_by_dir scopes context.build_dir
      ; config       = { loc = Loc.none; rules = [] }
      ; ocaml_flags  = None
      };
  t

let prefix_rules t prefix ~f =
  Build_system.prefix_rules t.build_system prefix ~f

let add_rule t ?sandbox ?mode ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  Build_system.add_rule t.build_system
    (Build_interpret.Rule.make ?sandbox ?mode ?locks ?loc
       ~context:(Some t.context) build)

let add_rule_get_targets t ?sandbox ?mode ?locks ?loc build =
  let build = Build.O.(>>>) build t.chdir in
  let rule =
    Build_interpret.Rule.make ?sandbox ?mode ?locks ?loc
      ~context:(Some t.context) build
  in
  Build_system.add_rule t.build_system rule;
  List.map rule.targets ~f:Build_interpret.Target.path

let add_rules t ?sandbox builds =
  List.iter builds ~f:(add_rule t ?sandbox)

let add_alias_deps t alias ?dyn_deps deps =
  Alias.add_deps t.build_system alias ?dyn_deps deps

let add_alias_action t alias ?locks ~stamp action =
  Alias.add_action t.build_system ~context:t.context alias ?locks ~stamp action

let eval_glob t ~dir re = Build_system.eval_glob t.build_system ~dir re
let load_dir t ~dir = Build_system.load_dir t.build_system ~dir
let on_load_dir t ~dir ~f = Build_system.on_load_dir t.build_system ~dir ~f

let source_files t ~src_path =
  match File_tree.find_dir t.file_tree src_path with
  | None -> String.Set.empty
  | Some dir -> File_tree.Dir.files dir

module Libs = struct
  open Build.O

  let gen_select_rules t ~dir compile_info =
    List.iter (Lib.Compile.resolved_selects compile_info) ~f:(fun rs ->
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

  let with_lib_deps t compile_info ~dir ~f =
    let prefix =
      Build.record_lib_deps (Lib.Compile.user_written_deps compile_info)
        ~kind:(if Lib.Compile.optional compile_info then
                 Optional
               else
                 Required)
    in
    let prefix =
      if t.context.merlin then
        Build.path (Path.relative dir ".merlin-exists")
        >>>
        prefix
      else
        prefix
    in
    prefix_rules t prefix ~f

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
           (lib_files_alias ~dir ~name:(Library.best_name lib) ~ext))
       |> Path.Set.of_list)

  let file_deps t libs ~ext =
    List.rev_map libs ~f:(fun (lib : Lib.t) ->
      if Lib.is_local lib then
        Alias.stamp_file
          (lib_files_alias ~dir:(Lib.src_dir lib) ~name:(Lib.name lib) ~ext)
      else
        Build_system.stamp_file_for_files_of t.build_system
          ~dir:(Lib.obj_dir lib) ~ext)
end

module Deps = struct
  open Build.O
  open Dep_conf

  let make_alias t ~scope ~dir s =
    let loc = String_with_vars.loc s in
    Alias.of_user_written_path ~loc
      (Path.relative ~error_loc:loc dir (expand_vars t ~scope ~dir s))

  let dep t ~scope ~dir = function
    | File  s ->
      let path = Path.relative ~error_loc:(String_with_vars.loc s) dir
                   (expand_vars t ~scope ~dir s) in
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
        let loc = String_with_vars.loc s in
        let path =
          Path.relative ~error_loc:loc dir (expand_vars t ~scope ~dir s) in
        match Glob_lexer.parse_string (Path.basename path) with
        | Ok re ->
          let dir = Path.parent_exn path in
          Build.paths_glob ~loc ~dir (Re.compile re)
        | Error (_pos, msg) ->
          Loc.fail loc "invalid glob: %s" msg
      end
    | Files_recursively_in s ->
      let path = Path.relative ~error_loc:(String_with_vars.loc s)
                   dir (expand_vars t ~scope ~dir s) in
      Build.files_recursively_in ~dir:path ~file_tree:t.file_tree
      >>^ Pset.to_list
    | Package p ->
      let pkg = Package.Name.of_string (expand_vars t ~scope ~dir p) in
      Alias.dep (Alias.package_install ~context:t.context ~pkg)
      >>^ fun () -> []
    | Universe ->
      Build.path Build_system.universe_file
      >>^ fun () -> []

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
        (sprintf "%s.version.sexp" (Package.Name.to_string p.name))
    in
    Build.Vspec.T (fn, (module V))

  let read sctx p = Build.vpath (spec sctx p)

  let set sctx p get =
    let spec = spec sctx p in
    add_rule sctx (get >>> Build.store_vfile spec);
    Build.vpath spec
end

module Scope_key = struct
  let of_string sctx key =
    match String.rsplit2 key ~on:'@' with
    | None ->
      (key, public_libs sctx)
    | Some (key, scope) ->
      ( key
      , Scope.libs (find_scope_by_name sctx (Dune_project.Name.decode scope)))

  let to_string key scope =
    sprintf "%s@%s" key (Dune_project.Name.encode scope)
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
      mutable ddeps     : (unit, Action.Var_expansion.t) Build.t String.Map.t
    }

  let add_lib_dep acc lib kind =
    acc.lib_deps <- String.Map.add acc.lib_deps lib kind

  let add_fail acc fail =
    acc.failures <- fail :: acc.failures;
    None

  let add_ddep acc ~key dep =
    acc.ddeps <- String.Map.add acc.ddeps key dep;
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
      ; lib_deps  = String.Map.empty
      ; sdeps     = Pset.empty
      ; ddeps     = String.Map.empty
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
          match Package.Name.Map.find (Scope.project scope).packages
                  (Package.Name.of_string s) with
          | Some p ->
            let x =
              Pkg_version.read sctx p >>^ function
              | None   -> Strings ([""], Concat)
              | Some s -> Strings ([s],  Concat)
            in
            add_ddep acc ~key x
          | None ->
            add_fail acc { fail = fun () ->
              Loc.fail loc "Package %S doesn't exist in the current project." s
            }
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
        | None -> String.Map.find extra_vars var
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
      match String.Map.find dynamic_expansions key with
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

  let run sctx ~loc ?(extra_vars=String.Map.empty)
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
      if Path.parent_exn target <> dir then
        Loc.fail loc
          "This action has targets in a different directory than the current \
           one, this is not allowed by Jbuilder at the moment:\n%s"
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
      let ddeps = String.Map.to_list forms.ddeps in
      Build.first (Build.all (List.map ddeps ~f:snd))
      >>^ (fun (vals, deps_written_by_user) ->
        let dynamic_expansions =
          List.fold_left2 ddeps vals ~init:String.Map.empty
            ~f:(fun acc (var, _) value -> String.Map.add acc var value)
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
