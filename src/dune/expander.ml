open Stdune

type expanded =
  | Static of Value.t list
  | Dynamic of Pform.Expansion.t

let static s = Ok (Static s)

type t =
  { dir : Path.Build.t
  ; hidden_env : Env.Var.Set.t
  ; env : Env.t
  ; lib_artifacts : Artifacts.Public_libs.t
  ; bin_artifacts_host : Artifacts.Bin.t
  ; ocaml_config : Value.t list String.Map.t Lazy.t
  ; bindings : Pform.Map.t
  ; scope : Scope.t
  ; c_compiler : string
  ; expand_var :
    t -> (expanded, User_message.t) Result.t option String_with_vars.expander
  }

let scope t = t.scope

let dir t = t.dir

let bindings t = t.bindings

let make_ocaml_config ocaml_config =
  let string s = [ Value.String s ] in
  Ocaml_config.to_list ocaml_config
  |> List.map ~f:(fun (k, v) ->
    ( k
    , match (v : Ocaml_config.Value.t) with
      | Bool x -> string (string_of_bool x)
      | Int x -> string (string_of_int x)
      | String x -> string x
      | Words x -> Value.L.strings x
      | Prog_and_args x -> Value.L.strings (x.prog :: x.args) ))
  |> String.Map.of_list_exn

let set_env t ~var ~value =
  { t with
    env = Env.add t.env ~var ~value
  ; hidden_env = Env.Var.Set.remove t.hidden_env var
  }

let hide_env t ~var = { t with hidden_env = Env.Var.Set.add t.hidden_env var }

let set_dir t ~dir = { t with dir }

let set_scope t ~scope = { t with scope }

let set_bin_artifacts t ~bin_artifacts_host = { t with bin_artifacts_host }

let extend_env t ~env =
  { t with
    env = Env.extend_env t.env env
  ; hidden_env = Env.Var.Set.diff t.hidden_env (Env.vars env)
  }

let add_bindings t ~bindings =
  { t with bindings = Pform.Map.superpose t.bindings bindings }

let expand_ocaml_config ocaml_config pform name =
  match String.Map.find ocaml_config name with
  | Some x -> x
  | None ->
    User_error.raise
      ~loc:(String_with_vars.Var.loc pform)
      [ Pp.textf "Unknown ocaml configuration variable %S" name ]

let expand_env t pform s : Value.t list option =
  match String.rsplit2 s ~on:'=' with
  | None ->
    User_error.raise
      ~loc:(String_with_vars.Var.loc pform)
      [ Pp.textf "%s must always come with a default value."
        (String_with_vars.Var.describe pform)
      ]
      ~hints:[ Pp.text "the syntax is %{env:VAR=DEFAULT-VALUE}" ]
  | Some (var, default) ->
    if Env.Var.Set.mem t.hidden_env var then
      None
    else
      Some [ String (Option.value ~default (Env.get t.env var)) ]

let expand_version scope pform s =
  match
    Package.Name.Map.find
      (Dune_project.packages (Scope.project scope))
      (Package.Name.of_string s)
  with
  | None ->
    User_error.raise
      ~loc:(String_with_vars.Var.loc pform)
      [ Pp.textf "Package %S doesn't exist in the current project." s ]
  | Some p -> (
    match p.version with
    | None -> [ Value.String "" ]
    | Some s -> [ String s ] )

let expand_var_exn t var syn =
  t.expand_var t var syn
  |> Option.map ~f:(function
    | Ok (Static s) -> s
      | Error msg -> raise (User_error.E msg)
       | Ok (Dynamic _) ->
         User_error.raise
           ~loc:(String_with_vars.Var.loc var)
           [ Pp.textf "%s isn't allowed in this position"
             (String_with_vars.Var.describe var)
           ])

let make ~scope ~(context : Context.t) ~lib_artifacts ~bin_artifacts_host =
  let expand_var
    ( { bindings
      ; ocaml_config
      ; env = _
      ; scope
      ; hidden_env = _
      ; dir = _
      ; bin_artifacts_host = _
      ; expand_var = _
      ; lib_artifacts = _
      ; c_compiler = _
      } as t ) var syntax_version =
    Pform.Map.expand bindings var syntax_version
    |> Option.bind ~f:(function
      | Pform.Expansion.Var (Values l) -> Some (static l)
        | Macro (Ocaml_config, s) ->
          Some (static (expand_ocaml_config (Lazy.force ocaml_config) var s))
         | Macro (Env, s) -> Option.map ~f:static (expand_env t var s)
         | Macro (Version, s) -> Some (static (expand_version scope var s))
         | Var Project_root ->
           Some (static [ Value.Dir (Path.build (Scope.root scope)) ])
         | expansion -> Some (Ok (Dynamic expansion)))
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
  ; lib_artifacts
  ; bin_artifacts_host
  ; expand_var
  ; c_compiler
  }

let expand t ~mode ~template =
  String_with_vars.expand ~dir:(Path.build t.dir) ~mode template
    ~f:(expand_var_exn t)

let expand_path t sw =
  expand t ~mode:Single ~template:sw
  |> Value.to_path ~error_loc:(String_with_vars.loc sw) ~dir:(Path.build t.dir)

let expand_str t sw =
  expand t ~mode:Single ~template:sw |> Value.to_string ~dir:(Path.build t.dir)

type reduced_var_result =
  | Unknown
  | Restricted
  | Expanded of Value.t list

let expand_with_reduced_var_set ~(context : Context.t) =
  let ocaml_config = lazy (make_ocaml_config context.ocaml_config) in
  let bindings = Pform.Map.create ~context in
  fun var syn ->
    match Pform.Map.expand bindings var syn with
    | None -> Unknown
    | Some (Var (Values l)) -> Expanded l
    | Some (Macro (Ocaml_config, s)) ->
      Expanded (expand_ocaml_config (Lazy.force ocaml_config) var s)
    | Some _ -> Restricted

module Resolved_forms = struct
  type t =
    { (* Failed resolutions *)
      mutable failures : Import.fail list
    ; (* All "name" for %{lib:name:...}/%{lib-available:name} forms *)
      mutable lib_deps : Lib_deps_info.t
    ; (* Static deps from %{...} variables. For instance %{exe:...} *)
      mutable sdeps : Path.Set.t
    ; (* Dynamic deps from %{...} variables. For instance %{read:...} *)
      mutable ddeps : (unit, Value.t list) Build.t String.Map.t
    }

  let failures t = t.failures

  let lib_deps t = t.lib_deps

  let sdeps t = t.sdeps

  let ddeps t = t.ddeps

  let empty () =
    { failures = []
    ; lib_deps = Lib_name.Map.empty
    ; sdeps = Path.Set.empty
    ; ddeps = String.Map.empty
    }

  let add_lib_dep acc lib kind =
    acc.lib_deps <- Lib_name.Map.set acc.lib_deps lib kind

  let add_fail acc fail =
    acc.failures <- fail :: acc.failures;
    None

  let add_ddep acc ~key dep =
    acc.ddeps <- String.Map.set acc.ddeps key dep;
    None
end

module Targets = struct
  type static =
    { targets : Path.Build.t list
    ; multiplicity : Dune_file.Rule.Targets.Multiplicity.t
    }

  type t =
    | Static of static
    | Infer
    | Forbidden of string
end

let path_exp path = [ Value.Path path ]

let str_exp str = [ Value.String str ]

let parse_lib_file ~loc s =
  match String.lsplit2 s ~on:':' with
  | None -> User_error.raise ~loc [ Pp.textf "invalid %%{lib:...} form: %s" s ]
  | Some (lib, f) -> (Lib_name.of_string_exn ~loc:(Some loc) lib, f)

type expansion_kind =
  | Dynamic
  | Static

let cc_of_c_flags t (cc : (unit, string list) Build.t C.Kind.Dict.t) =
  let open Build.O in
  C.Kind.Dict.map cc ~f:(fun cc ->
    cc >>^ fun flags -> Value.L.strings (t.c_compiler :: flags))

let resolve_binary t ~loc ~prog =
  match Artifacts.Bin.binary ~loc t.bin_artifacts_host prog with
  | Ok path -> Ok path
  | Error e ->
    Error { Import.fail = (fun () -> Action.Prog.Not_found.raise e) }

let expand_and_record acc ~map_exe ~dep_kind ~expansion_kind
  ~(dir : Path.Build.t) ~pform t expansion
    ~(cc : dir:Path.Build.t -> (unit, Value.t list) Build.t C.Kind.Dict.t) =
  let key = String_with_vars.Var.full_name pform in
  let loc = String_with_vars.Var.loc pform in
  let relative d s = Path.build (Path.Build.relative ~error_loc:loc d s) in
  let add_ddep =
    match expansion_kind with
    | Static ->
      fun _ ->
        User_error.raise ~loc
          [ Pp.textf "%s cannot be used in this position"
            (String_with_vars.Var.describe pform)
          ]
    | Dynamic -> Resolved_forms.add_ddep acc ~key
  in
  let open Build.O in
  match (expansion : Pform.Expansion.t) with
  | Var
    ( Project_root | First_dep | Deps | Targets | Target | Named_local
    | Values _ )
   |Macro ((Ocaml_config | Env | Version), _) ->
    assert false
  | Var Cc -> add_ddep (cc ~dir).c
  | Var Cxx -> add_ddep (cc ~dir).cxx
  | Macro (Path_no_dep, s) -> Some [ Value.Dir (relative dir s) ]
  | Macro (Exe, s) -> Some (path_exp (map_exe (relative dir s)))
  | Macro (Dep, s) -> Some (path_exp (relative dir s))
  | Macro (Bin, s) -> (
    match resolve_binary ~loc:(Some loc) t ~prog:s with
    | Error fail -> Resolved_forms.add_fail acc fail
    | Ok path -> Some (path_exp path) )
  | Macro (Lib, s) -> (
    let lib_dep, file = parse_lib_file ~loc s in
    Resolved_forms.add_lib_dep acc lib_dep dep_kind;
    match
      Artifacts.Public_libs.file_of_lib t.lib_artifacts ~loc ~lib:lib_dep ~file
    with
    | Ok path -> Some (path_exp path)
    | Error e -> Resolved_forms.add_fail acc { fail = (fun () -> raise e) } )
  | Macro (Libexec, s) -> (
    let lib_dep, file = parse_lib_file ~loc s in
    Resolved_forms.add_lib_dep acc lib_dep dep_kind;
    match
      Artifacts.Public_libs.file_of_lib t.lib_artifacts ~loc ~lib:lib_dep ~file
    with
    | Error e -> Resolved_forms.add_fail acc { fail = (fun () -> raise e) }
    | Ok path ->
      if (not Sys.win32) || Filename.extension s = ".exe" then
        Some (path_exp path)
      else
        let path_exe = Path.extend_basename path ~suffix:".exe" in
        let dep =
          Build.if_file_exists path_exe
            ~then_:(Build.path path_exe >>^ fun _ -> path_exp path_exe)
            ~else_:(Build.path path >>^ fun _ -> path_exp path)
        in
        add_ddep dep )
  | Macro (Lib_available, s) ->
    let lib = Lib_name.of_string_exn ~loc:(Some loc) s in
    Resolved_forms.add_lib_dep acc lib Optional;
    Lib.DB.available (Scope.libs t.scope) lib
    |> string_of_bool |> str_exp |> Option.some
  | Macro (Read, s) ->
    let path = relative dir s in
    let data = Build.contents path >>^ fun s -> [ Value.String s ] in
    add_ddep data
  | Macro (Read_lines, s) ->
    let path = relative dir s in
    let data = Build.lines_of path >>^ Value.L.strings in
    add_ddep data
  | Macro (Read_strings, s) ->
    let path = relative dir s in
    let data = Build.strings path >>^ Value.L.strings in
    add_ddep data

let check_multiplicity ~pform ~declaration ~use =
  let module Multiplicity = Dune_file.Rule.Targets.Multiplicity in
  let loc = String_with_vars.Var.loc pform in
  let error declaration use =
    User_error.raise ~loc
      [ Pp.textf
        "You can only use the variable %%{%s} if you defined the list of \
         targets using the field [%s] (not [%s])"
        use use declaration
      ]
  in
  match (declaration, use) with
  | Multiplicity.One, Multiplicity.One
   |Multiple, Multiple ->
    ()
  | One, Multiple -> error "target" "targets"
  | Multiple, One -> error "targets" "target"

let expand_and_record_deps acc ~(dir : Path.Build.t) ~dep_kind
  ~targets_written_by_user ~map_exe ~expand_var ~cc t pform syntax_version =
  let res =
    let targets ~(multiplicity : Dune_file.Rule.Targets.Multiplicity.t) =
      let loc = String_with_vars.Var.loc pform in
      match (targets_written_by_user : Targets.t) with
      | Infer ->
        User_error.raise ~loc
          [ Pp.textf "You cannot use %s with inferred rules."
            (String_with_vars.Var.describe pform)
          ]
      | Forbidden context ->
        User_error.raise ~loc
          [ Pp.textf "You cannot use %s in %s."
            (String_with_vars.Var.describe pform)
              context
          ]
      | Static { targets; multiplicity = declared_multiplicity } ->
        let value =
          List.map ~f:Path.build targets |> Value.L.dirs
          (* XXX hack to signal no dep *)
        in
        check_multiplicity ~pform ~declaration:declared_multiplicity
          ~use:multiplicity;
        Some value
    in
    expand_var t pform syntax_version
    |> Option.bind ~f:(function
      | Ok (Static s : expanded) -> Some s
        | Error msg -> raise (User_error.E msg)
         | Ok (Dynamic (expansion : Pform.Expansion.t)) -> (
           match expansion with
           | Var (Project_root | Values _)
            |Macro ((Ocaml_config | Env | Version), _) ->
             assert false (* these have been expanded statically *)
           | Var (First_dep | Deps | Named_local) -> None
           | Var Targets -> targets ~multiplicity:Multiple
           | Var Target -> targets ~multiplicity:One
           | _ ->
             expand_and_record acc ~map_exe ~dep_kind ~expansion_kind:Dynamic
               ~dir ~pform ~cc t expansion ))
  in
  Option.iter res ~f:(fun v ->
    acc.sdeps <-
      Path.Set.union (Path.Set.of_list (Value.L.deps_only v)) acc.sdeps);
  Option.map res ~f:static

let expand_no_ddeps acc ~dir ~dep_kind ~map_exe ~expand_var ~cc t pform
  syntax_version =
  let res =
    expand_var t pform syntax_version
    |> Option.bind ~f:(function
      | Ok (Static s : expanded) -> Some s
        | Error msg -> raise (User_error.E msg)
         | Ok (Dynamic (expansion : Pform.Expansion.t)) ->
           expand_and_record acc ~map_exe ~dep_kind ~cc ~expansion_kind:Static
             ~dir ~pform t expansion)
  in
  Option.iter res ~f:(fun v ->
    acc.sdeps <-
      Path.Set.union (Path.Set.of_list (Value.L.deps_only v)) acc.sdeps);
  Option.map res ~f:static

let gen_with_record_deps ~expand t resolved_forms ~dep_kind ~map_exe
  ~(c_flags : dir:Path.Build.t -> (unit, string list) Build.t C.Kind.Dict.t) =
  let cc ~dir = cc_of_c_flags t (c_flags ~dir) in
  let expand_var =
    expand
      (* we keep the dir constant here to replicate the old behavior of: (chdir
        foo %{exe:bar}). This should lookup ./bar rather than ./foo/bar *)
      resolved_forms ~dir:t.dir ~dep_kind ~map_exe ~expand_var:t.expand_var ~cc
  in
  let bindings =
    Pform.Map.of_list_exn [ ("cc", Pform.Var.Cc); ("cxx", Pform.Var.Cxx) ]
    |> Pform.Map.superpose t.bindings
  in
  { t with expand_var; bindings }

let with_record_deps t resolved_forms ~targets_written_by_user =
  let expand = expand_and_record_deps ~targets_written_by_user in
  gen_with_record_deps ~expand t resolved_forms

let with_record_no_ddeps = gen_with_record_deps ~expand:expand_no_ddeps

let expand_special_vars ~deps_written_by_user ~var pform =
  let key = String_with_vars.Var.full_name var in
  let loc = String_with_vars.Var.loc var in
  match pform with
  | Pform.Expansion.Var Named_local -> (
    match Bindings.find deps_written_by_user key with
    | None ->
      Code_error.raise "Local named variable not present in named deps"
        [ ("pform", String_with_vars.Var.to_dyn var)
        ; ( "deps_written_by_user"
          , Bindings.to_dyn Path.to_dyn deps_written_by_user )
        ]
    | Some x -> Value.L.paths x )
  | Var Deps -> deps_written_by_user |> Bindings.to_list |> Value.L.paths
  | Var First_dep -> (
    match deps_written_by_user with
    | Named _ :: _ ->
      (* This case is not possible: ${<} only exist in jbuild files and named
        dependencies are not available in jbuild files *)
      assert false
    | Unnamed v :: _ -> [ Path v ]
    | [] ->
      User_warning.emit ~loc
        [ Pp.textf "Variable '%s' used with no explicit dependencies" key ];
      [ Value.String "" ] )
  | _ ->
    Code_error.raise "Unexpected variable in step2"
      [ ("var", String_with_vars.Var.to_dyn var) ]

let expand_ddeps_and_bindings ~(dynamic_expansions : Value.t list String.Map.t)
  ~(deps_written_by_user : Path.t Bindings.t) ~expand_var t var syntax_version
    =
  let key = String_with_vars.Var.full_name var in
  ( match String.Map.find dynamic_expansions key with
  | Some v -> Some v
  | None ->
    expand_var t var syntax_version
    |> Option.map ~f:(function
      | Ok (Static v : expanded) -> v
        | Error msg -> raise (User_error.E msg)
         | Ok (Dynamic v) -> expand_special_vars ~deps_written_by_user ~var v)
  )
  |> Option.map ~f:static

let add_ddeps_and_bindings t ~dynamic_expansions ~deps_written_by_user =
  let expand_var =
    expand_ddeps_and_bindings ~dynamic_expansions ~deps_written_by_user
      ~expand_var:t.expand_var
  in
  { t with expand_var }

let expand_and_eval_set t set ~standard =
  let open Build.O in
  let dir = Path.build (dir t) in
  let standard =
    if Ordered_set_lang.Unexpanded.has_special_forms set then
      standard
    else
      Build.return []
  in
  let files =
    let f template =
      expand t ~mode:Single ~template
      |> Value.to_path ~error_loc:(String_with_vars.loc template) ~dir
    in
    Ordered_set_lang.Unexpanded.files set ~f
  in
  let expand =
    let f template = expand t ~mode:Many ~template in
    Ordered_set_lang.Unexpanded.expand ~dir ~f
  in
  let eval =
    let parse ~loc:_ s = s in
    Ordered_set_lang.eval ~parse ~eq:String.equal
  in
  match Path.Set.to_list files with
  | [] ->
    let set = expand set ~files_contents:Path.Map.empty in
    standard >>^ fun standard -> eval set ~standard
  | paths ->
    List.map paths ~f:Build.read_sexp
    |> Build.all |> Build.fanout standard
    >>^ fun (standard, sexps) ->
    let files_contents = List.combine paths sexps |> Path.Map.of_list_exn in
    expand set ~files_contents |> eval ~standard

let eval_blang t = function
  | Blang.Const x -> x (* common case *)
  | blang -> Blang.eval blang ~dir:(Path.build t.dir) ~f:(expand_var_exn t)
