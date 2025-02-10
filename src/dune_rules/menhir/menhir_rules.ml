open Import
open! Action_builder.O

(* This module interprets [(menhir ...)] stanzas -- that is, it provides build
   rules for Menhir parsers. *)

(* We assume that Menhir supports the commands [--infer-write-query] and
   [--infer-read-reply]. Although we could test at runtime whether this is the
   case, it is simpler to just require it. Dune is shipped with a constraint
   that Menhir (if present) is at version 20180523 or newer. *)

(* In order to perform OCaml type inference, we first let Menhir create a mock
   [.ml] file that contains just the semantic actions. We then use the OCaml
   compiler to perform type inference and create an inferred [.mli] file.
   Finally, we let Menhir read this [.mli] file. *)

(* This three-step process described is skipped in some cases where we know that
   OCaml type inference is not required, e.g., when Menhir is used in certain
   special modes, such as [--only-tokens]. In those cases, we use a
   straightforward one-step process. *)

(* -------------------------------------------------------------------------- *)

(* This signature describes the input of the functor [Run], which follows. *)

type stanza = Menhir_stanza.t

module type PARAMS = sig
  (* [cctx] is the compilation context. *)

  val cctx : Compilation_context.t

  (* [dir] is the directory inside [_build/<context>/...] where the build
     happens. If the [(menhir ...)] stanza appears in [src/dune], then [dir] is
     of the form [_build/<context>/src], e.g., [_build/default/src]. *)
  val dir : Path.Build.t

  (* [stanza] is the [(menhir ...)] stanza, as found in the [dune] file. *)

  val stanza : stanza
end

(* -------------------------------------------------------------------------- *)

(* This functor is where [(menhir ...)] stanzas are desugared. *)

let menhir_env =
  let f =
    Env_stanza_db_flags.flags
      ~name:"jsoo-env"
      ~root:(fun _ _ ->
        Menhir_env.map ~f:Action_builder.return Menhir_env.default |> Memo.return)
      ~f:(fun ~parent expander (local : Dune_env.config) ->
        let local = local.menhir in
        let open Memo.O in
        let+ parent = parent in
        let flags =
          Expander.expand_and_eval_set expander local.flags ~standard:parent.flags
        in
        { Menhir_env.flags; explain = Option.first_some local.explain parent.explain })
  in
  fun ~dir ->
    let open Memo.O in
    let* () = Memo.return () in
    (Staged.unstage f) dir
;;

module Run (P : PARAMS) = struct
  open P

  (* [sctx] is the "super context", while [context] is the "context". Both store
     information about the current build context. *)

  let sctx = Compilation_context.super_context cctx

  (* [build_dir] is the base directory of the context; we run menhir from this
     directory to we get correct error paths. *)
  let build_dir = Super_context.context sctx |> Context.build_dir
  let expander = Super_context.expander ~dir sctx
  let env = menhir_env ~dir

  let sandbox =
    let scope = Compilation_context.scope cctx in
    let project = Scope.project scope in
    if Dune_project.dune_version project < (3, 5)
    then Sandbox_config.default
    else Sandbox_config.needs_sandboxing
  ;;

  (* ------------------------------------------------------------------------ *)

  (* Naming conventions. *)

  (* If [m] is a (short) module name, such as "myparser", then [source m] is the
     corresponding source file, and [targets m] is the list of targets that
     Menhir must build. *)

  let source m = Path.relative (Path.build dir) (m ^ ".mly")

  let targets m ~cmly =
    let base = [ m ^ ".ml"; m ^ ".mli" ] in
    List.map ~f:(Path.Build.relative dir) (if cmly then (m ^ ".cmly") :: base else base)
  ;;

  let sources ms = List.map ~f:source ms

  (* The following definitions control where the mock [.ml] file and the
     inferred [.mli] file are created and how they are named. *)

  (* We change the module's base name, and use dummy extensions, so as to
     minimize the risk of confusing the build system (and the user). *)

  let mock m = m ^ "__mock"
  let mock_ml m : Path.Build.t = Path.Build.relative dir (mock m ^ ".ml.mock")
  let inferred_mli m : Path.Build.t = Path.Build.relative dir (mock m ^ ".mli.inferred")

  (* ------------------------------------------------------------------------ *)

  (* Rule generation. *)

  let menhir_binary =
    Super_context.resolve_program
      sctx
      ~dir
      ~where:Original_path
      "menhir"
      ~loc:None
      ~hint:"opam install menhir"
  ;;

  (* Reminder (from command.mli):

     [Deps] is for command line arguments that are dependencies. [As] is for
     command line arguments that are neither dependencies nor targets.
     [Hidden_targets] is for targets that are *not* command line arguments. *)

  type 'a args = 'a Command.Args.t list

  (* [menhir args] generates a Menhir command line (a build action). *)

  let menhir (args : 'a args) : Action.Full.t Action_builder.With_targets.t =
    Command.run_dyn_prog ~sandbox ~dir:(Path.build build_dir) menhir_binary args
  ;;

  let rule ?(mode = stanza.mode)
    : Action.Full.t Action_builder.With_targets.t -> unit Memo.t
    =
    Super_context.add_rule sctx ~dir ~mode ~loc:stanza.loc
  ;;

  let explain_flags base stanza =
    let open Memo.O in
    let* expander = expander
    and* env = env in
    let+ explain =
      match Option.first_some stanza.Menhir_stanza.explain env.Menhir_env.explain with
      | None -> Memo.return (stanza.menhir_syntax >= Menhir_stanza.explain_since)
      | Some explain -> Expander.eval_blang expander explain
    in
    if explain
    then
      [ Command.Args.A "--explain"
      ; Hidden_targets [ Path.Build.relative dir (base ^ ".conflicts") ]
      ]
    else []
  ;;

  let expand_flags flags =
    let open Memo.O in
    let+ env = env
    and+ expander = expander in
    Action_builder.memoize
      ~cutoff:(List.equal String.equal)
      "menhir flags"
      (Expander.expand_and_eval_set expander flags ~standard:env.Menhir_env.flags)
  ;;

  (* ------------------------------------------------------------------------ *)

  (* If there is no [base] clause, then a stanza that mentions several modules
     is equivalent to a list of stanzas, each of which mentions one module, so
     Menhir must be invoked once per module, separately. If there is a [base]
     clause, then the stanza describes a multi-module parser, so Menhir must be
     invoked once. In either case, we are able to reformulate the input in the
     form of a list of stanzas, each of which has a [base] clause. *)

  (* The current concrete name for [base] clauses is [merge_into], but I would
     like to change it in the future. *)

  let stanzas : stanza list =
    match stanza.merge_into with
    | None ->
      List.map
        ~f:(fun m -> { stanza with modules = [ m ]; merge_into = Some m })
        stanza.modules
    | Some _ -> [ stanza ]
  ;;

  (* ------------------------------------------------------------------------ *)

  (* The [--infer-*] commands should not be passed by the user; we take care of
     using these commands appropriately. Fail if they are present. *)

  let () =
    List.iter stanzas ~f:(fun (stanza : stanza) ->
      Ordered_set_lang.Unexpanded.fold_strings stanza.flags ~init:() ~f:(fun _pos sw () ->
        match String_with_vars.text_only sw with
        | None -> ()
        | Some "--explain" ->
          if stanza.menhir_syntax >= Menhir_stanza.explain_since
          then
            User_error.raise
              ~loc:(String_with_vars.loc sw)
              [ Pp.textf
                  "The Menhir '.conflicts' file is generated by default, so '--explain' \
                   should not be explicitly added to the list of Menhir flags."
              ]
        | Some text ->
          if
            List.mem
              ~equal:String.equal
              [ "--depend"
              ; "--raw-depend"
              ; "--infer"
              ; "--infer-write-query"
              ; "--infer-read-reply"
              ]
              text
          then
            User_error.raise
              ~loc:(String_with_vars.loc sw)
              [ Pp.textf "The flag %s must not be used in a menhir stanza." text ]))
  ;;

  (* ------------------------------------------------------------------------ *)

  (* [process3 stanza] converts a Menhir stanza into a set of build rules. This
     is the three-step process where Menhir is invoked twice and OCaml type
     inference is performed in between. *)

  let process3 base ~cmly (stanza : stanza) : unit Memo.t =
    let open Memo.O in
    let* expanded_flags = expand_flags stanza.flags in
    (* 1. A first invocation of Menhir creates a mock [.ml] file. *)
    let* () =
      menhir
        [ Command.Args.dyn expanded_flags
        ; Deps (sources stanza.modules)
        ; A "--base"
        ; Path (Path.relative (Path.build dir) base)
        ; A "--infer-write-query"
        ; Target (mock_ml base)
        ]
      |> rule ~mode:Standard
    in
    (* 2. The OCaml compiler performs type inference. *)
    let name = Module_name.of_string_allow_invalid (stanza.loc, mock base) in
    let mock_module : Module.t =
      let source =
        let impl = Module.File.make Dialect.ocaml (Path.build (mock_ml base)) in
        Module.Source.make ~impl [ name ]
      in
      Module.of_source ~visibility:Public ~kind:Impl source
    in
    let* mock_module =
      Pp_spec.pp_module_as
        (Compilation_context.preprocessing cctx)
        name
        mock_module
        ~lint:false
    in
    let cctx =
      Compilation_context.set_sandbox cctx Sandbox_config.needs_sandboxing
      |> Compilation_context.without_bin_annot
    in
    let* deps =
      Dep_rules.for_module (Compilation_context.ocamldep_modules_data cctx) mock_module
    in
    let* () =
      Module_compilation.ocamlc_i ~deps cctx mock_module ~output:(inferred_mli base)
    in
    let* explain_flags = explain_flags base stanza in
    (* 3. A second invocation of Menhir reads the inferred [.mli] file. *)
    menhir
      [ Command.Args.dyn expanded_flags
      ; S explain_flags
      ; Deps (sources stanza.modules)
      ; A "--base"
      ; Path (Path.relative (Path.build dir) base)
      ; A "--infer-read-reply"
      ; Dep (Path.build (inferred_mli base))
      ; Hidden_targets (targets base ~cmly)
      ]
    |> rule
  ;;

  (* ------------------------------------------------------------------------ *)

  (* [process3 stanza] converts a Menhir stanza into a set of build rules. This
     is a simpler one-step process where Menhir is invoked directly. *)

  let process1 base ~cmly (stanza : stanza) : unit Memo.t =
    let open Memo.O in
    let* expanded_flags = expand_flags stanza.flags
    and* explain_flags = explain_flags base stanza in
    menhir
      [ Command.Args.dyn expanded_flags
      ; S explain_flags
      ; Deps (sources stanza.modules)
      ; A "--base"
      ; Path (Path.relative (Path.build dir) base)
      ; Hidden_targets (targets base ~cmly)
      ]
    |> rule
  ;;

  (* ------------------------------------------------------------------------ *)

  (* [process stanza] converts a Menhir stanza into a set of build rules, using
     either [process3] or [process1], as appropriate. *)

  (* Because Menhir processes [--only-tokens] before the [--infer-*] commands,
     when [--only-tokens] is present, no [--infer-*] command should be used. *)

  let process (stanza : stanza) : unit Memo.t =
    let base = Option.value_exn stanza.merge_into in
    let ocaml_type_inference_disabled, cmly =
      Ordered_set_lang.Unexpanded.fold_strings
        stanza.flags
        ~init:(false, false)
        ~f:(fun pos sw ((only_tokens, cmly) as acc) ->
          match pos with
          | Neg -> acc
          | Pos ->
            (match String_with_vars.text_only sw with
             | Some "--only-tokens" -> true, cmly
             | Some "--cmly" -> only_tokens, true
             | Some _ | None -> acc))
    in
    if ocaml_type_inference_disabled || not stanza.infer
    then process1 base stanza ~cmly
    else process3 base stanza ~cmly
  ;;

  (* ------------------------------------------------------------------------ *)
  let gen_rules () = Memo.sequential_iter ~f:process stanzas
end

(* -------------------------------------------------------------------------- *)

(* The final glue. *)

let module_names (stanza : Menhir_stanza.t) : Module_name.t list =
  List.map (Menhir_stanza.modules stanza) ~f:(fun s ->
    (* TODO the loc can improved here *)
    Module_name.of_string_allow_invalid (stanza.loc, s))
;;

let gen_rules ~dir cctx stanza =
  let module R =
    Run (struct
      let cctx = cctx
      let dir = dir
      let stanza = stanza
    end)
  in
  R.gen_rules ()
;;
