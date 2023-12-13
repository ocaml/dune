open Import
open Action_builder.O
open Expander0
module Expanding_what = Expander0.Expanding_what
module Deps = Expander0.Deps

type value = Value.t list Deps.t

type t =
  { dir : Path.Build.t
  ; env : Env.t
  ; local_env : string Action_builder.t Env.Var.Map.t
  ; artifacts_host : Artifacts.t
  ; bindings : value Pform.Map.t
  ; scope : Scope.t
  ; context : Context.t
  ; expanding_what : Expanding_what.t
  }

type nonrec expansion_result =
  | Direct of value
  | Need_full_expander of (t -> value)

let scope t = t.scope
let artifacts t = t.artifacts_host
let dir t = t.dir
let context t = t.context

let set_local_env_var t ~var ~value =
  { t with local_env = Env.Var.Map.set t.local_env var value }
;;

let set_dir t ~dir = { t with dir }
let set_artifacts t ~artifacts_host = { t with artifacts_host }
let set_expanding_what t x = { t with expanding_what = x }

let map_exe t p =
  match t.expanding_what with
  | Deps_like_field -> p
  | Nothing_special | User_action _ | User_action_without_targets _ ->
    Context.map_exe t.context p
;;

let extend_env t ~env =
  (* [t.local_env] has precedence over [t.env], so we cannot extend [env] if
     there are already local bidings.. *)
  assert (Env.Var.Map.is_empty t.local_env);
  { t with env = Env.extend_env t.env env }
;;

let add_bindings_full t ~bindings =
  { t with bindings = Pform.Map.superpose bindings t.bindings }
;;

let add_bindings t ~bindings =
  add_bindings_full
    t
    ~bindings:(Pform.Map.map bindings ~f:(fun v -> Deps.Without (Memo.return v)))
;;

let string s = [ Value.String s ]
let strings l = Value.L.strings l

let relative ~source d s =
  Path.build (Path.Build.relative ~error_loc:(Dune_lang.Template.Pform.loc source) d s)
;;

let[@inline never] invalid_use_of_target_variable
  t
  ~(source : Dune_lang.Template.Pform.t)
  ~var_multiplicity
  =
  match t.expanding_what with
  | Nothing_special | Deps_like_field -> isn't_allowed_in_this_position ~source
  | User_action_without_targets { what } ->
    User_error.raise
      ~loc:source.loc
      [ Pp.textf
          "You cannot use %s in %s."
          (Dune_lang.Template.Pform.describe source)
          what
      ]
  | User_action targets ->
    (match targets with
     | Infer ->
       User_error.raise
         ~loc:source.loc
         [ Pp.textf
             "You cannot use %s with inferred rules."
             (Dune_lang.Template.Pform.describe source)
         ]
     | Static { targets = _; multiplicity } ->
       assert (multiplicity <> var_multiplicity);
       Targets_spec.Multiplicity.check_variable_matches_field
         ~loc:source.loc
         ~field:multiplicity
         ~variable:var_multiplicity;
       assert false)
;;

let expand_read_macro ~dir ~source s ~read ~pack =
  let path = relative ~source dir s in
  let read =
    let open Memo.O in
    let+ x =
      Build_system.read_file path ~f:(fun a -> Async.async (fun () -> read a))
      >>= Memo.of_reproducible_fiber
    in
    pack x
  in
  Need_full_expander
    (fun t ->
      if Dune_project.dune_version (Scope.project t.scope) >= (3, 0)
      then Without read
      else
        (* To prevent it from working in certain position before Dune 3.0. It'd
           be nice if we could invite the user to upgrade to (lang dune 3.0),
           but this is a bigger refactoring. *)
        With (Action_builder.of_memo read))
;;

let expand_pform_var ~source (var : Pform.Var.t) =
  match var with
  | Pkg _ -> assert false
  | User_var _
  | Deps
  | Input_file
  | Library_name
  | Partition
  | Impl_files
  | Intf_files
  | Inline_tests
  | Test
  | Corrected_suffix ->
    (* These would be part of [bindings] *)
    isn't_allowed_in_this_position ~source
  | First_dep ->
    (* This case is for %{<} which was only allowed inside jbuild files *)
    assert false
  | Target ->
    Need_full_expander
      (fun t -> invalid_use_of_target_variable t ~source ~var_multiplicity:One)
  | Targets ->
    Need_full_expander
      (fun t -> invalid_use_of_target_variable t ~source ~var_multiplicity:Multiple)
  | Ocaml
  | Ocaml_bin_dir
  | Ocamlc
  | Ocamlopt
  | Profile
  | Workspace_root
  | Context_name
  | Ignoring_promoted_rules
  | Nothing
  | Dev_null
  | Ocaml_stdlib_dir
  | Ext_obj
  | Ext_lib
  | Ext_dll
  | Ccomp_type
  | Make
  | Ext_exe
  | Cpp
  | Pa_cpp
  | Arch_sixtyfour
  | Ocaml_version
  | Ext_asm
  | Ext_plugin
  | Os_type
  | Architecture
  | Toolchain
  | System
  | Project_root
  | Cc
  | Cxx
  | Model -> assert false
;;

let env_macro t source macro_invocation =
  match Pform.Macro_invocation.Args.whole macro_invocation |> String.rsplit2 ~on:'=' with
  | None ->
    User_error.raise
      ~loc:source.Dune_lang.Template.Pform.loc
      [ Pp.textf
          "%s must always come with a default value."
          (Dune_lang.Template.Pform.describe source)
      ]
      ~hints:[ Pp.text "the syntax is %{env:VAR=DEFAULT-VALUE}" ]
  | Some (var, default) ->
    (match Env.Var.Map.find t.local_env var with
     | Some v -> Deps.With (v >>| string)
     | None ->
       Deps.Without (Env.get t.env var |> Option.value ~default |> string |> Memo.return))
;;

let expand_pform_macro ~dir ~source (macro_invocation : Pform.Macro_invocation.t) =
  let s = Pform.Macro_invocation.Args.whole macro_invocation in
  match macro_invocation.macro with
  | Pkg -> Code_error.raise "pkg forms aren't possible here" []
  | Pkg_self -> Code_error.raise "pkg-self forms aren't possible here" []
  | Bin
  | Bin_available
  | Lib _
  | Lib_available
  | Version
  | Dep
  | Exe
  | Coq_config
  | Artifact _
  | Ocaml_config -> assert false
  | Env -> Need_full_expander (fun t -> env_macro t source macro_invocation)
  | Path_no_dep ->
    (* This case is for %{path-no-dep:...} which was only allowed inside
           jbuild files *)
    assert false
  | Read -> expand_read_macro ~dir ~source s ~read:Io.read_file ~pack:string
  | Read_lines -> expand_read_macro ~dir ~source s ~read:Io.lines_of_file ~pack:strings
  | Read_strings ->
    expand_read_macro ~dir ~source s ~read:Io.lines_of_file ~pack:(fun lines ->
      List.map lines ~f:(fun line ->
        match Scanf.unescaped line with
        | Error () ->
          User_error.raise
            ~loc:(Loc.in_file (relative ~source dir s))
            [ Pp.textf
                "This file must be a list of lines escaped using OCaml's conventions"
            ]
        | Ok s -> s)
      |> strings)
;;

let expand_pform_gen ~bindings ~dir ~source (pform : Pform.t) : expansion_result =
  match Pform.Map.find bindings pform with
  | Some x -> Direct x
  | None ->
    let loc = Dune_lang.Template.Pform.loc source in
    (match Expander0.Source.expand loc pform with
     | Some (Direct v) -> Direct v
     | Some (Need_full_expander f) -> Direct (f (Expander0.make ~dir Nothing_special))
     | None ->
       (match pform with
        | Var var -> expand_pform_var ~source var
        | Macro macro_invocation -> expand_pform_macro ~dir ~source macro_invocation))
;;

(* Make sure to delay exceptions *)
let expand_pform_gen ~bindings ~dir ~source pform =
  match expand_pform_gen ~bindings ~source ~dir pform with
  | exception (User_error.E _ as exn) ->
    Direct
      (Without
         (let open Memo.O in
          let+ () = Memo.return () in
          reraise exn))
  | Direct _ as x -> x
  | Need_full_expander f ->
    Need_full_expander
      (fun t ->
        try f t with
        | User_error.E _ as exn ->
          Without
            (let open Memo.O in
             let+ () = Memo.return () in
             reraise exn))
;;

let describe_source ~source =
  Pp.textf
    "%s at %s"
    (Dune_lang.Template.Pform.to_string source)
    (Loc.to_file_colon_line source.loc)
;;

let expand_pform t ~source pform =
  Action_builder.push_stack_frame
    (fun () ->
      match
        match expand_pform_gen ~bindings:t.bindings ~dir:t.dir ~source pform with
        | Direct v -> v
        | Need_full_expander f -> f t
      with
      | With x -> x
      | Without x -> Action_builder.of_memo x)
    ~human_readable_description:(fun () -> describe_source ~source)
;;

let expand_pform_no_deps t ~source pform =
  Memo.push_stack_frame
    (fun () ->
      match
        match expand_pform_gen ~bindings:t.bindings ~dir:t.dir ~source pform with
        | Direct v -> v
        | Need_full_expander f -> f t
      with
      | With _ -> isn't_allowed_in_this_position ~source
      | Without x -> x)
    ~human_readable_description:(fun () -> describe_source ~source)
;;

let expand t ~mode template =
  String_expander.Action_builder.expand
    ~dir:(Path.build t.dir)
    ~mode
    template
    ~f:(expand_pform t)
;;

let make_root ~scope ~(context : Context.t) ~env ~artifacts_host =
  { dir = Context.build_dir context
  ; env
  ; local_env = Env.Var.Map.empty
  ; bindings = Pform.Map.empty
  ; scope
  ; artifacts_host
  ; context
  ; expanding_what = Nothing_special
  }
;;

let expand_path t sw =
  let+ v = expand t ~mode:Single sw in
  let loc = String_with_vars.loc sw in
  let path = Value.to_path v ~error_loc:loc ~dir:(Path.build t.dir) in
  let context_root = (Context.build_context t.context).build_dir in
  (match Path.as_in_build_dir path with
   | Some p when not (Path.Build.is_descendant p ~of_:context_root) ->
     (* TODO consider turning these into external paths, since we already allow
        them to be specified as absolute paths. *)
     User_error.raise ~loc [ Pp.text "path cannot escape the context root" ]
   | _ -> ());
  path
;;

let expand_str t sw =
  let+ v = expand t ~mode:Single sw in
  Value.to_string v ~dir:(Path.build t.dir)
;;

module No_deps = struct
  open Memo.O

  let expand_pform = expand_pform_no_deps

  let expand t ~mode sw =
    String_expander.Memo.expand ~dir:(Path.build t.dir) ~mode sw ~f:(expand_pform t)
  ;;

  let expand_path t sw =
    let+ v = expand t ~mode:Single sw in
    Value.to_path v ~error_loc:(String_with_vars.loc sw) ~dir:(Path.build t.dir)
  ;;

  let expand_str t sw =
    let+ v = expand t ~mode:Single sw in
    Value.to_string v ~dir:(Path.build t.dir)
  ;;
end

module With_deps_if_necessary = struct
  open Deps.O
  module E = String_with_vars.Make_expander (Deps)

  let expand_pform t ~source pform : _ Deps.t =
    match
      match expand_pform_gen ~bindings:t.bindings ~dir:t.dir ~source pform with
      | Direct v -> v
      | Need_full_expander f -> f t
    with
    | Without t ->
      Without
        (Memo.push_stack_frame
           (fun () -> t)
           ~human_readable_description:(fun () -> describe_source ~source))
    | With t ->
      With
        (Action_builder.push_stack_frame
           (fun () -> t)
           ~human_readable_description:(fun () -> describe_source ~source))
  ;;

  let expand t ~mode sw = E.expand ~dir:(Path.build t.dir) ~mode sw ~f:(expand_pform t)

  let expand_path t sw =
    let+ vs = expand t ~mode:Many sw in
    List.map vs ~f:(fun v ->
      Value.to_path_in_build_or_external v ~error_loc:(String_with_vars.loc sw) ~dir:t.dir)
  ;;
end

module With_reduced_var_set = struct
  open Memo.O

  let expand_pform_opt ~bindings ~dir ~source pform =
    let open Memo.O in
    Memo.push_stack_frame
      (fun () ->
        match expand_pform_gen ~bindings ~dir ~source pform with
        | Need_full_expander _ | Direct (With _) -> Memo.return None
        | Direct (Without x) -> x >>| Option.some)
      ~human_readable_description:(fun () -> describe_source ~source)
  ;;

  let expand_pform ~bindings ~dir ~source pform =
    expand_pform_opt ~bindings ~dir ~source pform
    >>| function
    | Some v -> v
    | None -> isn't_allowed_in_this_position ~source
  ;;

  let expand_str ~dir sw =
    let+ v =
      String_expander.Memo.expand
        ~dir:(Path.build dir)
        ~mode:Single
        sw
        ~f:(expand_pform ~bindings:Pform.Map.empty ~dir)
    in
    Value.to_string v ~dir:(Path.build dir)
  ;;

  let expand_str_partial ~dir sw =
    String_expander.Memo.expand_as_much_as_possible
      ~dir:(Path.build dir)
      sw
      ~f:(expand_pform_opt ~bindings:Pform.Map.empty ~dir)
  ;;

  let eval_blang ~dir blang =
    Blang_expand.eval
      ~f:(expand_pform ~bindings:Pform.Map.empty ~dir)
      ~dir:(Path.build dir)
      blang
  ;;
end

let expand_ordered_set_lang =
  let module Expander =
    Ordered_set_lang.Unexpanded.Expand (struct
      include Action_builder
      include String_expander.Action_builder
    end)
  in
  Expander.expand
;;

let expand_and_eval_set t set ~standard =
  let dir = Path.build (dir t) in
  let+ standard =
    if Ordered_set_lang.Unexpanded.has_special_forms set
    then standard
    else Action_builder.return []
  and+ set = expand_ordered_set_lang set ~dir ~f:(expand_pform t) in
  Ordered_set_lang.eval set ~standard ~eq:String.equal ~parse:(fun ~loc:_ s -> s)
;;

let eval_blang t blang =
  Blang_expand.eval ~f:(No_deps.expand_pform t) ~dir:(Path.build t.dir) blang
;;

let expand_lock ~base expander (Locks.Lock sw) =
  let open Memo.O in
  match base with
  | `Of_expander -> No_deps.expand_path expander sw
  | `This base ->
    let+ str = No_deps.expand_str expander sw in
    Path.relative base str
;;

let expand_locks ~base expander locks =
  Memo.List.map locks ~f:(expand_lock ~base expander) |> Action_builder.of_memo
;;
