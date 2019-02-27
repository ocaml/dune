open Stdune

type t =
  { dir : Path.t
  ; hidden_env : Env.Var.Set.t
  ; env : Env.t
  ; artifacts : Artifacts.t
  ; artifacts_host : Artifacts.t
  ; ocaml_config : Value.t list String.Map.t Lazy.t
  ; bindings : Pform.Map.t
  ; scope : Scope.t
  ; c_compiler : string
  ; expand_var :
      t -> (Value.t list, Pform.Expansion.t)
             result option String_with_vars.expander
  }

let scope t = t.scope
let dir t = t.dir
let bindings t = t.bindings
let artifacts_host t = t.artifacts_host

let make_ocaml_config ocaml_config =
  let string s = [Value.String s] in
  Ocaml_config.to_list ocaml_config
  |> List.map  ~f:(fun (k, v) ->
    ( k
    , match (v : Ocaml_config.Value.t) with
    | Bool          x -> string (string_of_bool x)
    | Int           x -> string (string_of_int x)
    | String        x -> string x
    | Words         x -> Value.L.strings x
    | Prog_and_args x -> Value.L.strings (x.prog :: x.args)))
  |> String.Map.of_list_exn

let set_env t ~var ~value =
  { t with
    env = Env.add t.env ~var ~value
  ; hidden_env = Env.Var.Set.remove t.hidden_env var
  }

let hide_env t ~var =
  { t with hidden_env = Env.Var.Set.add t.hidden_env var }

let set_dir t ~dir =
  { t with dir }

let set_scope t ~scope =
  { t with scope }

let set_artifacts t ~artifacts ~artifacts_host =
  { t with artifacts ; artifacts_host }

let extend_env t ~env =
  { t with
    env = Env.extend_env t.env env
  ; hidden_env = Env.Var.Set.diff t.hidden_env (Env.vars env)
  }

let add_bindings t ~bindings =
  { t with
    bindings = Pform.Map.superpose t.bindings bindings
  }

let expand_ocaml_config ocaml_config pform name =
  match String.Map.find ocaml_config name with
  | Some x -> x
  | None ->
    Errors.fail (String_with_vars.Var.loc pform)
      "Unknown ocaml configuration variable %S"
      name

let expand_env t pform s : Value.t list option =
  match String.rsplit2 s ~on:'=' with
  | None ->
    Errors.fail (String_with_vars.Var.loc pform)
      "%s must always come with a default value\n\
       Hint: the syntax is %%{env:VAR=DEFAULT-VALUE}"
      (String_with_vars.Var.describe pform)
  | Some (var, default) ->
    if Env.Var.Set.mem t.hidden_env var then
      None
    else
      Some [String (Option.value ~default (Env.get t.env var))]

let expand_var_exn t var syn =
  t.expand_var t var syn
  |> Option.map ~f:(function
    | Ok s -> s
    | Error _ ->
      Errors.fail (String_with_vars.Var.loc var)
        "%s isn't allowed in this position"
        (String_with_vars.Var.describe var))

let make ~scope ~(context : Context.t) ~artifacts
      ~artifacts_host =
  let expand_var ({ bindings; ocaml_config; env = _; scope
                  ; hidden_env = _
                  ; dir = _ ; artifacts = _; expand_var = _
                  ; artifacts_host = _; c_compiler = _ } as t)
        var syntax_version =
    Pform.Map.expand bindings var syntax_version
    |> Option.bind ~f:(function
      | Pform.Expansion.Var (Values l) -> Some (Ok l)
      | Macro (Ocaml_config, s) ->
        Some (Ok (expand_ocaml_config (Lazy.force ocaml_config) var s))
      | Macro (Env, s) -> Option.map ~f:Result.ok (expand_env t var s)
      | Var Project_root -> Some (Ok [Value.Dir (Scope.root scope)])
      | expansion -> Some (Error expansion))
  in
  let ocaml_config = lazy (make_ocaml_config context.ocaml_config) in
  let dir = context.build_dir in
  let bindings = Pform.Map.create ~context in
  let env = context.env in
  let c_compiler = context.c_compiler in
  { dir
  ; hidden_env = Env.Var.Set.empty
  ; env
  ; ocaml_config
  ; bindings
  ; scope
  ; artifacts
  ; artifacts_host
  ; expand_var
  ; c_compiler
  }

let expand t ~mode ~template =
  String_with_vars.expand ~dir:t.dir ~mode template
    ~f:(expand_var_exn t)

let expand_path t sw =
  expand t ~mode:Single ~template:sw
  |> Value.to_path ~error_loc:(String_with_vars.loc sw) ~dir:t.dir

let expand_str t sw =
  expand t ~mode:Single ~template:sw
  |> Value.to_string ~dir:t.dir

module Resolved_forms = struct
  type t =
    { (* Failed resolutions *)
      mutable failures  : Import.fail list
    ; (* All "name" for %{lib:name:...}/%{lib-available:name} forms *)
      mutable lib_deps  : Lib_deps_info.t
    ; (* Static deps from %{...} variables. For instance %{exe:...} *)
      mutable sdeps     : Path.Set.t
    ; (* Dynamic deps from %{...} variables. For instance %{read:...} *)
      mutable ddeps     : (unit, Value.t list) Build.t String.Map.t
    }

  let failures t = t.failures
  let lib_deps t = t.lib_deps
  let sdeps t = t.sdeps
  let ddeps t = t.ddeps

  let empty () =
    { failures  = []
    ; lib_deps  = Lib_name.Map.empty
    ; sdeps     = Path.Set.empty
    ; ddeps     = String.Map.empty
    }

  let add_lib_dep acc lib kind =
    acc.lib_deps <- Lib_name.Map.add acc.lib_deps lib kind

  let add_fail acc fail =
    acc.failures <- fail :: acc.failures;
    None

  let add_ddep acc ~key dep =
    acc.ddeps <- String.Map.add acc.ddeps key dep;
    None
end

module Targets = struct
  type t =
    | Static of Path.t list
    | Infer
    | Forbidden of string
end

let path_exp path = [Value.Path path]
let str_exp  str  = [Value.String str]

let parse_lib_file ~loc s =
  match String.lsplit2 s ~on:':' with
  | None ->
    Errors.fail loc "invalid %%{lib:...} form: %s" s
  | Some (lib, f) -> (Lib_name.of_string_exn ~loc:(Some loc) lib, f)

type dynamic =
  { read_package : Package.t -> (unit, string option) Build.t
  }

let error_expansion_kind =
  { read_package = fun _ -> assert false
  }

type expansion_kind =
  | Dynamic of dynamic
  | Static

let cc_of_c_flags t cc ~dir =
  let open Build.O in
  cc ~dir >>^ fun flags ->
  Value.L.strings (t.c_compiler :: flags)

let cxx_of_cxx_flags = cc_of_c_flags

let expand_and_record acc ~map_exe ~dep_kind ~scope
      ~expansion_kind ~dir ~pform t expansion
      ~cxx ~cc =
  let key = String_with_vars.Var.full_name pform in
  let loc = String_with_vars.Var.loc pform in
  let relative = Path.relative ~error_loc:loc in
  let add_ddep =
    match expansion_kind with
    | Static -> fun _ ->
      Errors.fail loc "%s cannot be used in this position"
        (String_with_vars.Var.describe pform)
    | Dynamic _ -> Resolved_forms.add_ddep acc ~key
  in
  let { read_package } =
    match expansion_kind with
    | Static -> error_expansion_kind
    | Dynamic d -> d
  in
  let open Build.O in
  match (expansion : Pform.Expansion.t) with
  | Var (Project_root | First_dep | Deps | Targets | Named_local | Values _)
  | Macro ((Ocaml_config | Env ), _) -> assert false
  | Var Cc -> add_ddep (cc ~dir)
  | Var Cxx -> add_ddep (cxx ~dir)
  | Macro (Path_no_dep, s) -> Some [Value.Dir (relative dir s)]
  | Macro (Exe, s) -> Some (path_exp (map_exe (relative dir s)))
  | Macro (Dep, s) -> Some (path_exp (relative dir s))
  | Macro (Bin, s) -> begin
      match Artifacts.binary ~loc:(Some loc) t.artifacts_host s with
      | Ok path -> Some (path_exp path)
      | Error e ->
        Resolved_forms.add_fail acc
          ({ fail = fun () -> Action.Prog.Not_found.raise e })
    end
  | Macro (Lib, s) -> begin
      let lib_dep, file = parse_lib_file ~loc s in
      Resolved_forms.add_lib_dep acc lib_dep dep_kind;
      match
        Artifacts.file_of_lib t.artifacts ~loc ~lib:lib_dep ~file
      with
      | Ok path -> Some (path_exp path)
      | Error fail -> Resolved_forms.add_fail acc fail
    end
  | Macro (Libexec, s) -> begin
      let lib_dep, file = parse_lib_file ~loc s in
      Resolved_forms.add_lib_dep acc lib_dep dep_kind;
      match
        Artifacts.file_of_lib t.artifacts ~loc ~lib:lib_dep ~file
      with
      | Error fail -> Resolved_forms.add_fail acc fail
      | Ok path ->
        if not Sys.win32 || Filename.extension s = ".exe" then begin
          Some (path_exp path)
        end else begin
          let path_exe = Path.extend_basename path ~suffix:".exe" in
          let dep =
            Build.if_file_exists path_exe
              ~then_:(Build.path path_exe >>^ fun _ ->
                      path_exp path_exe)
              ~else_:(Build.path path >>^ fun _ ->
                      path_exp path)
          in
          add_ddep dep
        end
    end
  | Macro (Lib_available, s) -> begin
      let lib = Lib_name.of_string_exn ~loc:(Some loc) s in
      Resolved_forms.add_lib_dep acc lib Optional;
      Lib.DB.available (Scope.libs t.scope) lib
      |> string_of_bool
      |> str_exp
      |> Option.some
    end
  | Macro (Lib_intf, s) ->
    let lib_name =
      match String.lsplit2 s ~on:':' with
      | None -> Errors.fail loc "Invalid intf form %s" key
      | Some (lib_name, "public") ->
        Lib_name.of_string_exn ~loc:(Some loc) lib_name
      | Some (_, s) ->
        Errors.fail loc
          "Invalid intf form %s. \
          Only public %%{intf:%s:public is supported}"
          key s
    in
    Resolved_forms.add_lib_dep acc lib_name dep_kind;
    let db = Scope.libs scope in
    begin match Lib.DB.find db lib_name with
    | Error reason ->
      Resolved_forms.add_fail acc (
        { fail = fun () ->
          Lib.not_available ~loc reason
            "Public library %a" Lib_name.pp_quoted lib_name }
      )
    | Ok lib ->
      let dir = Lib.public_cmi_dir lib in
      let cmis =
        Build.paths_matching ~loc ~dir (Path.test_extension ~ext:".cmi")
        >>^ Value.L.of_paths_set
      in
      add_ddep cmis
    end
  | Macro (Read, s) -> begin
      let path = relative dir s in
      let data =
        Build.contents path
        >>^ fun s -> [Value.String s]
      in
      add_ddep data
    end
  | Macro (Read_lines, s) -> begin
      let path = relative dir s in
      let data =
        Build.lines_of path
        >>^ Value.L.strings
      in
      add_ddep data
    end
  | Macro (Read_strings, s) -> begin
      let path = relative dir s in
      let data =
        Build.strings path
        >>^ Value.L.strings
      in
      add_ddep data
    end
  | Macro (Version, s) -> begin
      match Package.Name.Map.find
              (Dune_project.packages (Scope.project scope))
              (Package.Name.of_string s) with
      | Some p ->
        let open Build.O in
        let x =
          read_package p >>^ function
          | None   -> [Value.String ""]
          | Some s -> [String s]
        in
        add_ddep x
      | None ->
        Resolved_forms.add_fail acc { fail = fun () ->
          Errors.fail loc
            "Package %S doesn't exist in the current project." s
        }
    end

let expand_and_record_deps acc ~dir ~read_package ~dep_kind
      ~targets_written_by_user ~map_exe ~expand_var ~cc ~cxx
      t pform syntax_version =
  let res =
    expand_var t pform syntax_version
    |> Option.bind ~f:(function
      | Ok s -> Some s
      | Error (expansion : Pform.Expansion.t) ->
        match expansion with
        | Var (Project_root | Values _)
        | Macro ((Ocaml_config | Env), _) ->
          assert false (* these have been expanded statically *)
        | Var (First_dep | Deps | Named_local) -> None
        | Var Targets ->
          let loc = String_with_vars.Var.loc pform in
          begin match (targets_written_by_user : Targets.t) with
          | Infer ->
            Errors.fail loc "You cannot use %s with inferred rules."
              (String_with_vars.Var.describe pform)
          | Forbidden context ->
            Errors.fail loc "You cannot use %s in %s."
              (String_with_vars.Var.describe pform) context
          | Static l ->
            Some (Value.L.dirs l) (* XXX hack to signal no dep *)
          end
        | _ ->
          expand_and_record acc ~map_exe ~dep_kind ~scope:t.scope
            ~expansion_kind:(Dynamic { read_package }) ~dir ~pform
            ~cc ~cxx
            t expansion
    )
  in
  Option.iter res ~f:(fun v ->
    acc.sdeps <- Path.Set.union
                   (Path.Set.of_list (Value.L.deps_only v)) acc.sdeps
  );
  Option.map res ~f:Result.ok

let expand_no_ddeps acc ~dir ~dep_kind ~map_exe ~expand_var
      ~cc ~cxx
      t pform syntax_version =
  let res =
    expand_var t pform syntax_version
    |> Option.bind ~f:(function
      | Ok s -> Some s
      | Error (expansion : Pform.Expansion.t) ->
        expand_and_record acc ~map_exe ~dep_kind ~scope:t.scope
          ~cc ~cxx ~expansion_kind:Static ~dir ~pform t expansion)
  in
  Option.iter res ~f:(fun v ->
    acc.sdeps <- Path.Set.union
                   (Path.Set.of_list (Value.L.deps_only v)) acc.sdeps
  );
  Option.map res ~f:Result.ok

let gen_with_record_deps ~expand t resolved_forms ~dep_kind ~map_exe
      ~c_flags ~cxx_flags =
  let cc = cc_of_c_flags t c_flags in
  let cxx = cxx_of_cxx_flags t cxx_flags in
  let expand_var =
    expand
      (* we keep the dir constant here to replicate the old behavior of: (chdir
         foo %{exe:bar}). This should lookup ./bar rather than ./foo/bar *)
      resolved_forms
      ~dir:t.dir ~dep_kind
      ~map_exe
      ~expand_var:t.expand_var ~cc ~cxx in
  let bindings =
    Pform.Map.of_list_exn
      [ "cc", Pform.Var.Cc
      ; "cxx", Pform.Var.Cxx
      ]
    |> Pform.Map.superpose t.bindings
  in
  { t with
    expand_var
  ; bindings
  }

let with_record_deps t resolved_forms ~read_package ~targets_written_by_user =
  let expand =
    expand_and_record_deps ~read_package ~targets_written_by_user in
  gen_with_record_deps ~expand t resolved_forms

let with_record_no_ddeps =
  gen_with_record_deps ~expand:expand_no_ddeps

let expand_special_vars ~deps_written_by_user ~var pform =
  let key = String_with_vars.Var.full_name var in
  let loc = String_with_vars.Var.loc var in
  match pform with
  | Pform.Expansion.Var Named_local ->
    begin match Bindings.find deps_written_by_user key with
    | None ->
      Exn.code_error "Local named variable not present in named deps"
        [ "pform", String_with_vars.Var.to_sexp var
        ; "deps_written_by_user",
          Bindings.to_sexp Path.to_sexp deps_written_by_user
        ]
    | Some x -> Value.L.paths x
    end
  | Var Deps ->
    deps_written_by_user
    |> Bindings.to_list
    |> Value.L.paths
  | Var First_dep ->
    begin match deps_written_by_user with
    | Named _ :: _ ->
      (* This case is not possible: ${<} only exist in jbuild
         files and named dependencies are not available in
         jbuild files *)
      assert false
    | Unnamed v :: _ -> [Path v]
    | [] ->
      Errors.warn loc "Variable '%s' used with no explicit \
                       dependencies@." key;
      [Value.String ""]
    end
  | _ ->
    Exn.code_error "Unexpected variable in step2"
      ["var", String_with_vars.Var.to_sexp var]

let expand_ddeps_and_bindings ~(dynamic_expansions : Value.t list String.Map.t)
      ~(deps_written_by_user : Path.t Bindings.t) ~expand_var
      t var syntax_version =
  let key = String_with_vars.Var.full_name var in
  (match String.Map.find dynamic_expansions key with
   | Some v -> Some v
   | None ->
     expand_var t var syntax_version
     |> Option.map ~f:(function
       | Error v -> expand_special_vars ~deps_written_by_user ~var v
       | Ok v -> v))
  |> Option.map ~f:Result.ok

let add_ddeps_and_bindings t ~dynamic_expansions ~deps_written_by_user =
  let expand_var =
    expand_ddeps_and_bindings ~dynamic_expansions ~deps_written_by_user
      ~expand_var:t.expand_var
  in
  { t with expand_var }

let expand_and_eval_set t set ~standard =
  let open Build.O in
  let dir = dir t in
  let parse ~loc:_ s = s in
  let (syntax, files) =
    let f template =
      expand t ~mode:Single ~template
      |> Value.to_path ~error_loc:(String_with_vars.loc template) ~dir
    in
    Ordered_set_lang.Unexpanded.files set ~f in
  let f template = expand t ~mode:Many ~template in
  match Path.Set.to_list files with
  | [] ->
    let set =
      Ordered_set_lang.Unexpanded.expand set ~dir
        ~files_contents:Path.Map.empty ~f
    in
    standard >>^ fun standard ->
    Ordered_set_lang.String.eval set ~standard ~parse
  | paths ->
    List.map paths ~f:(fun f -> Build.read_sexp f syntax)
    |> Build.all
    |> Build.fanout standard
    >>^ fun (standard, sexps) ->
    let files_contents = List.combine paths sexps |> Path.Map.of_list_exn in
    Ordered_set_lang.Unexpanded.expand set ~dir ~files_contents ~f
    |> Ordered_set_lang.String.eval ~standard ~parse

let eval_blang t = function
  | Blang.Const x -> x (* common case *)
  | blang -> Blang.eval blang ~dir:t.dir ~f:(expand_var_exn t)

module Option = struct
  exception Not_found

  let expand_var_exn t var syn =
    t.expand_var t var syn
    |> Option.map ~f:(function
      | Ok s -> s
      | Error _ -> raise_notrace Not_found)

  let expand t ~mode ~template =
    match
      String_with_vars.expand ~dir:t.dir ~mode template
        ~f:(expand_var_exn t)
    with
    | exception Not_found -> None
    | s -> Some s

  let expand_path t sw =
    expand t ~mode:Single ~template:sw
    |> Option.map ~f:(
      Value.to_path ~error_loc:(String_with_vars.loc sw) ~dir:t.dir)

  let expand_str t sw =
    expand t ~mode:Single ~template:sw
    |> Option.map ~f:(Value.to_string ~dir:t.dir)
end
