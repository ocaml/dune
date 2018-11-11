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
  ; expand_var :
      t -> (Value.t list, Pform.Expansion.t)
             result option String_with_vars.expander
  }

type var_expander =
  (Value.t list, Pform.Expansion.t) result option String_with_vars.expander

let scope t = t.scope
let dir t = t.dir
let bindings t = t.bindings

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
      ~artifacts_host ~cxx_flags =
  let expand_var ({ bindings; ocaml_config; env = _; scope
                  ; hidden_env = _
                  ; dir = _ ; artifacts = _; expand_var = _
                  ; artifacts_host = _ } as t)
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
  let bindings = Pform.Map.create ~context ~cxx_flags in
  let env = context.env in
  { dir
  ; hidden_env = Env.Var.Set.empty
  ; env
  ; ocaml_config
  ; bindings
  ; scope
  ; artifacts
  ; artifacts_host
  ; expand_var
  }

let expand t ~mode ~template =
  String_with_vars.expand ~dir:t.dir ~mode template ~f:(fun var syn ->
    expand_var_exn t var syn)

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
    acc.failures <- fail :: acc.failures

  let add_ddep acc ~key dep =
    acc.ddeps <- String.Map.add acc.ddeps key dep
end

type targets =
  | Static of Path.t list
  | Infer
  | Alias

let path_exp path = [Value.Path path]
let str_exp  str  = [Value.String str]

let parse_lib_file ~loc s =
  match String.lsplit2 s ~on:':' with
  | None ->
    Errors.fail loc "invalid %%{lib:...} form: %s" s
  | Some (lib, f) -> (Lib_name.of_string_exn ~loc:(Some loc) lib, f)

let expand_and_record_static acc ~map_exe ~dir ~pform t expansion =
  let loc = String_with_vars.Var.loc pform in
  let relative = Path.relative ~error_loc:loc in
  match (expansion : Pform.Expansion.t) with
  | Macro (Path_no_dep, s) -> Some [Value.Dir (relative dir s)]
  | Macro (Exe, s) -> Some (path_exp (map_exe (relative dir s)))
  | Macro (Dep, s) -> Some (path_exp (relative dir s))
  | Macro (Bin, s) -> begin
      match Artifacts.binary ~loc:(Some loc) t.artifacts_host s with
      | Ok path -> Some (path_exp path)
      | Error e ->
        Resolved_forms.add_fail acc
          ({ fail = fun () -> Action.Prog.Not_found.raise e });
        None
    end
  | _ -> None

let expand_and_record_lib_deps acc ~dep_kind ~pform t expansion =
  let loc = String_with_vars.Var.loc pform in
  let key = String_with_vars.Var.full_name pform in
  let scope = scope t in
  let open Build.O in
  match (expansion : Pform.Expansion.t) with
  | Macro (Lib, s) -> begin
      let lib_dep, file = parse_lib_file ~loc s in
      Resolved_forms.add_lib_dep acc lib_dep dep_kind;
      match
        Artifacts.file_of_lib t.artifacts ~loc ~lib:lib_dep ~file
      with
      | Ok path -> Some (path_exp path)
      | Error fail -> Resolved_forms.add_fail acc fail; None
    end
  | Macro (Libexec, s) -> begin
      let lib_dep, file = parse_lib_file ~loc s in
      Resolved_forms.add_lib_dep acc lib_dep dep_kind;
      match
        Artifacts.file_of_lib t.artifacts ~loc ~lib:lib_dep ~file
      with
      | Error fail -> Resolved_forms.add_fail acc fail; None
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
          Resolved_forms.add_ddep acc ~key dep;
          None
        end
    end
  | Macro (Lib_available, s) -> begin
      let lib = Lib_name.of_string_exn ~loc:(Some loc) s in
      Resolved_forms.add_lib_dep acc lib Optional;
      Some (str_exp (string_of_bool (
        Lib.DB.available (Scope.libs scope) lib)))
    end
  | _ -> None

let expand_and_record_ddeps acc ~dir ~scope ~read_package ~pform expansion =
  let key = String_with_vars.Var.full_name pform in
  let loc = String_with_vars.Var.loc pform in
  let relative = Path.relative ~error_loc:loc in
  let open Build.O in
  match (expansion : Pform.Expansion.t) with
  | Macro (Read, s) -> begin
      let path = relative dir s in
      let data =
        Build.contents path
        >>^ fun s -> [Value.String s]
      in
      Resolved_forms.add_ddep acc ~key data
    end
  | Macro (Read_lines, s) -> begin
      let path = relative dir s in
      let data =
        Build.lines_of path
        >>^ Value.L.strings
      in
      Resolved_forms.add_ddep acc ~key data
    end
  | Macro (Read_strings, s) -> begin
      let path = relative dir s in
      let data =
        Build.strings path
        >>^ Value.L.strings
      in
      Resolved_forms.add_ddep acc ~key data
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
        Resolved_forms.add_ddep acc ~key x
      | None ->
        Resolved_forms.add_fail acc { fail = fun () ->
          Errors.fail loc
            "Package %S doesn't exist in the current project." s
        }
    end
  | _ -> ()

let expand_and_record_deps acc ~dir ~read_package ~dep_kind
      ~targets_written_by_user ~map_exe ~expand_var
      t pform syntax_version =
  let res =
    expand_var t pform syntax_version
    |> Option.bind ~f:(function
      | Ok s -> Some s
      | Error (expansion : Pform.Expansion.t) ->
        expand_and_record_ddeps acc ~dir ~scope:t.scope ~read_package ~pform expansion;
        match expand_and_record_lib_deps acc ~dep_kind ~pform t expansion with
        | Some s -> Some s
        | None ->
          match expand_and_record_static acc ~map_exe ~dir
                  ~pform t expansion with
          | Some s -> Some s
          | None ->
            match expansion with
            | Var (Project_root | Values _)
            | Macro ((Ocaml_config | Env), _) ->
              assert false (* these have been expanded statically *)
            | Var (First_dep | Deps | Named_local) -> None
            | Var Targets ->
              let loc = String_with_vars.Var.loc pform in
              begin match targets_written_by_user with
              | Infer ->
                Errors.fail loc "You cannot use %s with inferred rules."
                  (String_with_vars.Var.describe pform)
              | Alias ->
                Errors.fail loc "You cannot use %s in aliases."
                  (String_with_vars.Var.describe pform)
              | Static l ->
                Some (Value.L.dirs l) (* XXX hack to signal no dep *)
              end
            | _ -> None)
  in
  Option.iter res ~f:(fun v ->
    acc.sdeps <- Path.Set.union
                   (Path.Set.of_list (Value.L.deps_only v)) acc.sdeps
  );
  Option.map res ~f:Result.ok

let expand_no_read acc ~dir ~dep_kind ~map_exe ~expand_var
      t pform syntax_version =
  let res =
    let not_allowed pform =
      let loc = String_with_vars.Var.loc pform in
      Errors.fail loc "You cannot use %s in this position"
        (String_with_vars.Var.describe pform)
    in
    expand_var t pform syntax_version
    |> Option.bind ~f:(function
      | Ok s -> Some s
      | Error (expansion : Pform.Expansion.t) ->
        match expand_and_record_lib_deps acc ~dep_kind ~pform t expansion with
        | Some s -> Some s
        | None ->
          match expand_and_record_static acc ~map_exe ~dir
                  ~pform t expansion with
          | Some s -> Some s
          | None ->
            match expansion with
            | Var (Project_root | Values _)
            | Macro ((Ocaml_config | Env), _) ->
              assert false (* these have been expanded statically *)
            | Var (First_dep | Deps | Named_local)
            | Var Targets -> not_allowed pform
            | _ -> None)
  in
  Option.iter res ~f:(fun v ->
    acc.sdeps <- Path.Set.union
                   (Path.Set.of_list (Value.L.deps_only v)) acc.sdeps
  );
  Option.map res ~f:Result.ok

let with_record_deps t resolved_forms ~read_package ~dep_kind
      ~targets_written_by_user ~map_exe =
  let expand_var =
    expand_and_record_deps
      (* we keep the dir constant here to replicate the old behavior of: (chdir
         foo %{exe:bar}). This should lookup ./bar rather than ./foo/bar *)
      ~dir:t.dir
      resolved_forms ~read_package ~dep_kind
      ~expand_var:t.expand_var ~targets_written_by_user ~map_exe in
  { t with expand_var }

let with_record_no_read_deps t resolved_forms ~dep_kind
      ~map_exe =
  let expand_var =
    expand_no_read
      (* we keep the dir constant here to replicate the old behavior of: (chdir
         foo %{exe:bar}). This should lookup ./bar rather than ./foo/bar *)
      ~dir:t.dir resolved_forms ~dep_kind ~expand_var:t.expand_var ~map_exe
  in
  { t with expand_var }

let expand_special_vars ~deps_written_by_user ~var expansion =
  let key = String_with_vars.Var.full_name var in
  let loc = String_with_vars.Var.loc var in
  match expansion with
  | Ok v -> v (* in some cases we'll expand %{env} here *)
  | Error pform ->
    begin match pform with
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
    end

let expand_ddeps_and_bindings ~(dynamic_expansions : Value.t list String.Map.t)
      ~(deps_written_by_user : Path.t Bindings.t) ~expand_var
      t var syntax_version =
  let key = String_with_vars.Var.full_name var in
  match String.Map.find dynamic_expansions key with
  | Some v -> Some (Ok v)
  | None ->
    begin match expand_var t var syntax_version with
    | None -> None
    | Some v -> Some (Ok (expand_special_vars ~deps_written_by_user ~var v))
    end

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

