open! Dune_engine
open Import

module Expanded = struct
  type t =
    | Value of Value.t list
    | Deferred of Pform.Expansion.t
    | Unknown
    | Error of User_message.t

  let to_value_opt t ~on_deferred =
    match t with
    | Value v -> Some v
    | Deferred d -> on_deferred d
    | Error m -> raise (User_error.E m)
    | Unknown -> None

  let of_value_opt = function
    | None -> Unknown
    | Some v -> Value v
end

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
  ; context : Context.t
  ; expand_var : t -> Expanded.t String_with_vars.expander
  ; artifacts_dynamic : bool
  ; lookup_artifacts : (dir:Path.Build.t -> Ml_sources.Artifacts.t) option
  ; map_exe : Path.t -> Path.t
  ; foreign_flags :
      dir:Path.Build.t -> string list Build.t Foreign_language.Dict.t
  ; find_package : Package.Name.t -> Package.t option
  }

let scope t = t.scope

let artifacts t = t.bin_artifacts_host

let dir t = t.dir

let context t = t.context

let make_ocaml_config ocaml_config =
  let string s = [ Value.String s ] in
  Ocaml_config.to_list ocaml_config
  |> String.Map.of_list_map_exn ~f:(fun (k, v) ->
         ( k
         , match (v : Ocaml_config.Value.t) with
           | Bool x -> string (string_of_bool x)
           | Int x -> string (string_of_int x)
           | String x -> string x
           | Words x -> Value.L.strings x
           | Prog_and_args x -> Value.L.strings (x.prog :: x.args) ))

let set_foreign_flags t ~f:foreign_flags = { t with foreign_flags }

let set_env t ~var ~value =
  { t with
    env = Env.add t.env ~var ~value
  ; hidden_env = Env.Var.Set.remove t.hidden_env var
  }

let hide_env t ~var = { t with hidden_env = Env.Var.Set.add t.hidden_env var }

let set_dir t ~dir = { t with dir }

let set_scope t ~scope = { t with scope }

let set_bin_artifacts t ~bin_artifacts_host = { t with bin_artifacts_host }

let set_artifacts_dynamic t artifacts_dynamic = { t with artifacts_dynamic }

let set_lookup_ml_sources t ~f = { t with lookup_artifacts = Some f }

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

let isn't_allowed_in_this_position pform =
  let loc = String_with_vars.Var.loc pform in
  User_error.raise ~loc
    [ Pp.textf "%s isn't allowed in this position"
        (String_with_vars.Var.describe pform)
    ]

let expand_var_exn t var syn =
  t.expand_var t var syn
  |> Expanded.to_value_opt ~on_deferred:(fun _ ->
         isn't_allowed_in_this_position var)

let expand_artifact ~dir ~loc t a s : Expanded.t =
  let path = Path.Build.relative dir s in
  let name = Path.Build.basename path in
  let dir = Path.Build.parent_exn path in
  match t.lookup_artifacts with
  | None -> Unknown
  | Some lookup -> (
    let does_not_exist ~loc ~what name =
      Expanded.Error
        (User_error.make ~loc [ Pp.textf "%s %s does not exist." what name ])
    in
    let artifacts = lookup ~dir in
    match a with
    | Pform.Artifact.Mod kind -> (
      let name = Module_name.of_string_allow_invalid (loc, name) in
      match Ml_sources.Artifacts.lookup_module artifacts name with
      | None -> does_not_exist ~loc ~what:"Module" (Module_name.to_string name)
      | Some (t, m) -> (
        match Obj_dir.Module.cm_file t m ~kind with
        | None -> Value [ Value.String "" ]
        | Some path -> Value [ Value.Path (Path.build path) ] ) )
    | Lib mode -> (
      let name = Lib_name.parse_string_exn (loc, name) in
      match Ml_sources.Artifacts.lookup_library artifacts name with
      | None -> does_not_exist ~loc ~what:"Library" (Lib_name.to_string name)
      | Some lib ->
        let archive =
          Dune_file.Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext mode)
        in
        Value [ Value.Path (Path.build archive) ] ) )

(* This expansion function only expands the most "static" variables and macros.
   These are all known without building anything, evaluating any dune files, and
   they do not introduce any dependencies. *)
let static_expand
    ({ ocaml_config; bindings; dir; scope; artifacts_dynamic; _ } as t) var
    syntax_version : Expanded.t =
  match Pform.Map.expand bindings var syntax_version with
  | None -> Unknown
  | Some expand -> (
    match expand with
    | Pform.Expansion.Var (Values l) -> Value l
    | Macro (Ocaml_config, s) ->
      Value (expand_ocaml_config (Lazy.force ocaml_config) var s)
    | Macro (Env, s) -> Expanded.of_value_opt (expand_env t var s)
    | Macro (Version, s) -> Value (expand_version scope var s)
    | Var Project_root -> Value [ Value.Dir (Path.build (Scope.root scope)) ]
    | Macro (Artifact a, s) when not artifacts_dynamic ->
      let loc = String_with_vars.Var.loc var in
      expand_artifact ~dir ~loc t a s
    | expansion -> Deferred expansion )

let cc_cxx_bindings =
  Pform.Map.of_list_exn [ ("cc", Pform.Var.Cc); ("cxx", Pform.Var.Cxx) ]

let make ~scope ~(context : Context.t) ~lib_artifacts ~bin_artifacts_host
    ~find_package =
  let ocaml_config = lazy (make_ocaml_config context.ocaml_config) in
  let dir = context.build_dir in
  let bindings =
    let context = Pform.Map.create ~context in
    Pform.Map.superpose context cc_cxx_bindings
  in
  let env = context.env in
  let c_compiler = Ocaml_config.c_compiler context.ocaml_config in
  { dir
  ; hidden_env = Env.Var.Set.empty
  ; env
  ; ocaml_config
  ; bindings
  ; scope
  ; lib_artifacts
  ; bin_artifacts_host
  ; expand_var = static_expand
  ; c_compiler
  ; context
  ; artifacts_dynamic = false
  ; lookup_artifacts = None
  ; (* For dependency field expansion, we do not expand dependencies to the host
       context *)
    map_exe = Context.map_exe context
  ; foreign_flags =
      (fun ~dir ->
        Code_error.raise "foreign flags expander is not set"
          [ ("dir", Path.Build.to_dyn dir) ])
  ; find_package
  }

let expand t ~mode ~template =
  String_with_vars.expand ~dir:(Path.build t.dir) ~mode template
    ~f:(expand_var_exn t)

let expand_path t sw =
  expand t ~mode:Single ~template:sw
  |> Value.to_path ~error_loc:(String_with_vars.loc sw) ~dir:(Path.build t.dir)

let expand_str t sw =
  expand t ~mode:Single ~template:sw |> Value.to_string ~dir:(Path.build t.dir)

module Or_exn = struct
  let expand t ~mode ~template =
    match
      String_with_vars.expand ~dir:(Path.build t.dir) ~mode template
        ~f:(expand_var_exn t)
    with
    | x -> Ok x
    | exception (User_error.E _ as e) -> Error e

  let expand_path t sw =
    expand t ~mode:Single ~template:sw
    |> Result.map
         ~f:
           (Value.to_path ~error_loc:(String_with_vars.loc sw)
              ~dir:(Path.build t.dir))

  let expand_str t sw =
    expand t ~mode:Single ~template:sw
    |> Result.map ~f:(Value.to_string ~dir:(Path.build t.dir))
end

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
      mutable failure : Import.fail option
    ; (* All "name" for %{lib:name:...}/%{lib-available:name} forms *)
      mutable lib_deps : Lib_deps_info.t
    ; (* Static deps from %{...} variables. For instance %{exe:...} *)
      mutable sdeps : Path.Set.t
    ; (* Dynamic deps from %{...} variables. For instance %{read:...} *)
      mutable ddeps : Value.t list Build.t Pform.Expansion.Map.t
    }

  let create () =
    { failure = None
    ; lib_deps = Lib_name.Map.empty
    ; sdeps = Path.Set.empty
    ; ddeps = Pform.Expansion.Map.empty
    }

  let add_lib_dep acc lib kind =
    acc.lib_deps <- Lib_name.Map.set acc.lib_deps lib kind

  let add_value_opt t =
    Option.iter ~f:(fun v ->
        t.sdeps <-
          Path.Set.union (Path.Set.of_list (Value.L.deps_only v)) t.sdeps)

  let to_build t =
    let open Build.O in
    let ddeps = Pform.Expansion.Map.to_list t.ddeps in
    let+ () = Build.record_lib_deps t.lib_deps
    and+ () = Build.path_set t.sdeps
    and+ values = Build.all (List.map ddeps ~f:snd)
    and+ () =
      match t.failure with
      | None -> Build.return ()
      | Some fail -> Build.fail fail
    in
    List.fold_left2 ddeps values ~init:Pform.Expansion.Map.empty
      ~f:(fun acc (var, _) value -> Pform.Expansion.Map.add_exn acc var value)
end

let path_exp path = [ Value.Path path ]

let str_exp str = [ Value.String str ]

let parse_lib_file ~loc s =
  match String.lsplit2 s ~on:':' with
  | None -> User_error.raise ~loc [ Pp.textf "invalid %%{lib:...} form: %s" s ]
  | Some (lib, f) -> (Lib_name.parse_string_exn (loc, lib), f)

let cc t ~dir =
  let open Build.O in
  let cc = t.foreign_flags ~dir in
  Foreign_language.Dict.map cc ~f:(fun cc ->
      let+ flags = cc in
      Value.L.strings (t.c_compiler :: flags))

type expand_result =
  | Static of Value.t list
  | Dynamic of Value.t list Build.t

let expand_and_record_generic acc ~dep_kind ~(dir : Path.Build.t) ~pform t
    expansion =
  let loc = String_with_vars.Var.loc pform in
  let relative d s = Path.build (Path.Build.relative ~error_loc:loc d s) in
  let open Build.O in
  match (expansion : Pform.Expansion.t) with
  | Var
      ( Project_root | First_dep | Deps | Targets | Target | Named_local
      | Values _ )
  | Macro ((Ocaml_config | Env | Version), _) ->
    assert false
  | Var Cc -> Dynamic (cc t ~dir).c
  | Var Cxx -> Dynamic (cc t ~dir).cxx
  | Macro (Artifact a, s) ->
    let data =
      Build.dyn_paths
        (let+ values =
           Build.delayed (fun () ->
               match expand_artifact ~dir ~loc t a s with
               | Value v -> v
               | Error msg -> raise (User_error.E msg)
               | Unknown
               | Deferred _ ->
                 isn't_allowed_in_this_position pform)
         in
         (values, Value.L.deps_only values))
    in
    Dynamic data
  | Macro (Path_no_dep, s) -> Static [ Value.Dir (relative dir s) ]
  | Macro (Exe, s) -> Static (path_exp (t.map_exe (relative dir s)))
  | Macro (Dep, s) -> Static (path_exp (relative dir s))
  | Macro (Bin, s) ->
    Static
      ( Artifacts.Bin.binary ~loc:(Some loc) t.bin_artifacts_host s
      |> Action.Prog.ok_exn |> path_exp )
  | Macro (Lib { lib_exec; lib_private }, s) -> (
    let lib, file = parse_lib_file ~loc s in
    Resolved_forms.add_lib_dep acc lib dep_kind;
    match
      if lib_private then
        let open Result.O in
        let* lib = Lib.DB.resolve (Scope.libs t.scope) (loc, lib) in
        let current_project_name = Scope.project t.scope |> Dune_project.name
        and referenced_project_name =
          Lib.info lib |> Lib_info.status |> Lib_info.Status.project_name
        in
        if
          (* TODO This breaks scoped packages and projects *)
          Option.equal Dune_project.Name.equal (Some current_project_name)
            referenced_project_name
        then
          Ok (Path.relative (Lib_info.src_dir (Lib.info lib)) file)
        else
          Error
            (User_error.E
               (User_error.make ~loc
                  [ Pp.textf
                      "The variable \"lib-private\" can only refer to \
                       libraries within the same project. The current \
                       project's name is %S, but the reference is to %s."
                      (Dune_project.Name.to_string_hum current_project_name)
                      ( match referenced_project_name with
                      | Some name ->
                        "\"" ^ Dune_project.Name.to_string_hum name ^ "\""
                      | None -> "an external library" )
                  ]))
      else
        Artifacts.Public_libs.file_of_lib t.lib_artifacts ~loc ~lib ~file
    with
    | Ok path ->
      (* TODO: The [exec = true] case is currently not handled correctly and
         does not match the documentation. *)
      if (not lib_exec) || (not Sys.win32) || Filename.extension s = ".exe" then
        Static (path_exp path)
      else
        let path_exe = Path.extend_basename path ~suffix:".exe" in
        let dep =
          Build.if_file_exists path_exe
            ~then_:
              (let+ () = Build.path path_exe in
               path_exp path_exe)
            ~else_:
              (let+ () = Build.path path in
               path_exp path)
        in
        Dynamic dep
    | Error e ->
      raise
        ( match lib_private with
        | true -> e
        | false ->
          if Lib.DB.available (Scope.libs t.scope) lib then
            User_error.E
              (User_error.make ~loc
                 [ Pp.textf
                     "The library %S is not public. The variable \"lib\" \
                      expands to the file's installation path which is not \
                      defined for private libraries."
                     (Lib_name.to_string lib)
                 ])
          else
            e ) )
  | Macro (Lib_available, s) ->
    let lib = Lib_name.parse_string_exn (loc, s) in
    Resolved_forms.add_lib_dep acc lib Optional;
    Static
      (Lib.DB.available (Scope.libs t.scope) lib |> string_of_bool |> str_exp)
  | Macro (Read, s) ->
    let path = relative dir s in
    let data =
      let+ s = Build.contents path in
      [ Value.String s ]
    in
    Dynamic data
  | Macro (Read_lines, s) ->
    let path = relative dir s in
    let data = Build.map (Build.lines_of path) ~f:Value.L.strings in
    Dynamic data
  | Macro (Read_strings, s) ->
    let path = relative dir s in
    let data = Build.map (Build.strings path) ~f:Value.L.strings in
    Dynamic data

let gen_with_record_deps ~expand t resolved_forms ~dep_kind =
  let expand_var =
    expand
      (* we keep the dir constant here to replicate the old behavior of: (chdir
         foo %{exe:bar}). This should lookup ./bar rather than ./foo/bar *)
      resolved_forms ~dir:t.dir ~dep_kind ~expand_var:t.expand_var
  in
  { t with expand_var }

module Deps_like : sig
  val expand_deps_like_field :
    t -> dep_kind:Lib_deps_info.Kind.t -> f:(t -> 'a Build.t) -> 'a Build.t
end = struct
  let expand_and_record_static acc ~dep_kind ~(dir : Path.Build.t) ~pform t
      expansion =
    match expand_and_record_generic acc ~dep_kind ~dir ~pform t expansion with
    | Static l -> l
    | Dynamic _ -> isn't_allowed_in_this_position pform

  let expand_no_ddeps acc ~dir ~dep_kind ~expand_var t pform syntax_version =
    let res =
      expand_var t pform syntax_version
      |> Expanded.to_value_opt ~on_deferred:(fun exp ->
             Some (expand_and_record_static acc ~dep_kind ~dir ~pform t exp))
    in
    Resolved_forms.add_value_opt acc res;
    Expanded.of_value_opt res

  let expand_deps_like_field t ~dep_kind ~f =
    let open Build.O in
    let forms = Resolved_forms.create () in
    let t = gen_with_record_deps ~expand:expand_no_ddeps t forms ~dep_kind in
    let t = { t with map_exe = Fun.id } in
    let build = f t in
    let+ x = build
    and+ dynamic_expansions = Resolved_forms.to_build forms in
    if Pform.Expansion.Map.is_empty dynamic_expansions then
      x
    else
      Code_error.raise "ddeps are not allowed in this position" []
end

include Deps_like

module Action_like : sig
  val expand_action :
       t
    -> deps_written_by_user:Path.t Bindings.t Build.t
    -> targets_written_by_user:Targets.Or_forbidden.t
    -> dep_kind:Lib_deps_info.Kind.t
    -> partial:(t -> 'a)
    -> final:(t -> 'a -> 'b)
    -> 'a * 'b Build.t
end = struct
  (* Expand variables that correspond to user defined variables, deps, and
     target(s) fields *)
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

  (* Responsible for the 2nd phase expansions of dynamic dependencies. After
     we've discovered all Build.t values, waited for them to build, we can
     finally substitute them back form the dynamic_expansions map *)
  let expand_ddeps_and_bindings
      ~(dynamic_expansions : Value.t list Pform.Expansion.Map.t)
      ~(deps_written_by_user : Path.t Bindings.t) ~expand_var t var
      syntax_version =
    let open Option.O in
    (let* key = Pform.Map.expand t.bindings var syntax_version in
     match Pform.Expansion.Map.find dynamic_expansions key with
     | Some v -> Some v
     | None ->
       expand_var t var syntax_version
       |> Expanded.to_value_opt ~on_deferred:(fun exp ->
              Some (expand_special_vars ~deps_written_by_user ~var exp)))
    |> Expanded.of_value_opt

  let add_ddeps_and_bindings t ~dynamic_expansions ~deps_written_by_user =
    let expand_var =
      expand_ddeps_and_bindings ~dynamic_expansions ~deps_written_by_user
        ~expand_var:t.expand_var
    in
    { t with expand_var }

  (* Expansion where every Dynamic value is not expanded and recorded to be
     substituted later. *)
  let expand_and_record_dynamic acc ~dep_kind ~(dir : Path.Build.t) ~pform t
      expansion =
    match expand_and_record_generic acc ~dep_kind ~dir ~pform t expansion with
    | Static l -> Some l
    | Dynamic dep ->
      acc.ddeps <- Pform.Expansion.Map.set acc.ddeps expansion dep;
      None
    | exception (User_error.E _ as e) ->
      acc.failure <- Some { fail = (fun () -> raise e) };
      None

  let expand_and_record_deps acc ~(dir : Path.Build.t) ~dep_kind
      ~targets_written_by_user ~expand_var t pform syntax_version =
    let res =
      let targets ~(multiplicity : Targets.Multiplicity.t) =
        let loc = String_with_vars.Var.loc pform in
        match (targets_written_by_user : Targets.Or_forbidden.t) with
        | Targets.Or_forbidden.Targets Infer ->
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
        | Targets.Or_forbidden.Targets
            (Static { targets; multiplicity = field_multiplicity }) ->
          Targets.Multiplicity.check_variable_matches_field ~loc
            ~field:field_multiplicity ~variable:multiplicity;
          (* XXX hack to signal no dep *)
          List.map ~f:Path.build targets |> Value.L.dirs
      in
      expand_var t pform syntax_version
      |> Expanded.to_value_opt
           ~on_deferred:(fun (expansion : Pform.Expansion.t) ->
             match expansion with
             | Var (Project_root | Values _)
             | Macro ((Ocaml_config | Env | Version), _) ->
               assert false (* these have been expanded statically *)
             | Var (First_dep | Deps | Named_local) -> None
             | Var Targets -> Some (targets ~multiplicity:Multiple)
             | Var Target -> Some (targets ~multiplicity:One)
             | _ ->
               expand_and_record_dynamic acc ~dep_kind ~dir ~pform t expansion)
    in
    Resolved_forms.add_value_opt acc res;
    Expanded.of_value_opt res

  let expand_action t ~deps_written_by_user ~targets_written_by_user ~dep_kind
      ~partial ~final =
    let open Build.O in
    let forms = Resolved_forms.create () in
    let expand = expand_and_record_deps ~targets_written_by_user in
    let x =
      let t = gen_with_record_deps ~expand t forms ~dep_kind in
      partial t
    in
    let y =
      let+ dynamic_expansions = Resolved_forms.to_build forms
      and+ deps_written_by_user = deps_written_by_user in
      let t =
        add_ddeps_and_bindings t ~dynamic_expansions ~deps_written_by_user
      in
      final t x
    in
    (x, y)
end

include Action_like

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
    let+ standard = standard in
    eval set ~standard
  | paths ->
    let+ standard = standard
    and+ sexps = Build.all (List.map paths ~f:Build.read_sexp) in
    let files_contents = List.combine paths sexps |> Path.Map.of_list_exn in
    expand set ~files_contents |> eval ~standard

let eval_blang t = function
  | Blang.Const x -> x (* common case *)
  | blang -> Blang.eval blang ~dir:(Path.build t.dir) ~f:(expand_var_exn t)

let map_exe t = t.map_exe

let find_package t pkg = t.find_package pkg
