open Import
open! No_io

module SC = Super_context

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

(* This three-step process described is skipped in some cases where we know
   that OCaml type inference is not required, e.g., when Menhir is used in
   certain special modes, such as [--only-tokens]. In those cases, we use a
   straightforward one-step process. *)

(* -------------------------------------------------------------------------- *)

(* This signature describes the input of the functor [Run], which follows. *)

type stanza =
  Dune_file.Menhir.t

module type PARAMS = sig

  (* [cctx] is the compilation context. *)

  val cctx: Compilation_context.t

  (* [stanza] is the [(menhir ...)] stanza, as found in the [jbuild] file. *)

  val stanza: stanza

end

(* -------------------------------------------------------------------------- *)

(* This functor is where [(menhir ...)] stanzas are desugared. *)

module Run (P : PARAMS) : sig end = struct

  open P

  (* [sctx] is the "super context", while [context] is the "context". Both
     store information about the current build context. *)

  let sctx =
    Compilation_context.super_context cctx

  (* [dir] is the directory inside [_build/<context>/...] where the build
     happens. If the [(menhir ...)] stanza appears in [src/jbuild], then [dir]
     is of the form [_build/<context>/src], e.g., [_build/default/src]. *)

  let dir =
    Compilation_context.dir cctx

  (* ------------------------------------------------------------------------ *)

  (* Naming conventions. *)

  (* If [m] is a (short) module name, such as "myparser", then [source m] is
     the corresponding source file, and [targets m] is the list of targets
     that Menhir must build. *)

  let source m =
    Path.relative dir (m ^ ".mly")

  let targets m =
    List.map ~f:(Path.relative dir) [m ^ ".ml"; m ^ ".mli"]

  let sources ms =
    List.map ~f:source ms

  (* The following definitions control where the mock [.ml] file and the
     inferred [.mli] file are created and how they are named. *)

  (* We change the module's base name, and use dummy extensions, so as to
     minimize the risk of confusing the build system (and the user). *)

  let mock m =
    m ^ "__mock"

  let mock_ml m : Path.t =
    Path.relative dir (mock m ^ ".ml.mock")

  let inferred_mli m : Path.t =
    Path.relative dir (mock m ^ ".mli.inferred")

  (* ------------------------------------------------------------------------ *)

  (* Rule generation. *)

  let menhir_binary =
    SC.resolve_program sctx "menhir" ~loc:None ~hint:"try: opam install menhir"

  (* Reminder (from arg_spec.mli):

     [Deps]           is for command line arguments that are dependencies.
     [As]             is for command line arguments
                      that are neither dependencies nor targets.
     [Hidden_targets] is for targets that are *not* command line arguments.  *)

  type args =
    unit Arg_spec.t list

  (* [menhir args] generates a Menhir command line (a build action). *)

  let menhir (args : args) : (unit, Action.t) Build.t =
    Build.run ~dir menhir_binary args

  let rule : (unit, Action.t) Build.t -> unit =
    SC.add_rule sctx ~mode:stanza.mode ~loc:stanza.loc

  let rule_menhir args =
    rule (menhir args)

  (* ------------------------------------------------------------------------ *)

  (* If there is no [base] clause, then a stanza that mentions several modules
     is equivalent to a list of stanzas, each of which mentions one module, so
     Menhir must be invoked once per module, separately. If there is a [base]
     clause, then the stanza describes a multi-module parser, so Menhir must
     be invoked once. In either case, we are able to reformulate the input in
     the form of a list of stanzas, each of which has a [base] clause. *)

  (* The current concrete name for [base] clauses is [merge_into], but I would
     like to change it in the future. *)

  let stanzas : stanza list =
    match stanza.merge_into with
    | None ->
        List.map ~f:(fun m ->
          { stanza with modules = [ m ]; merge_into = Some m }
        ) stanza.modules
    | Some _ ->
        [ stanza ]

  (* ------------------------------------------------------------------------ *)

  (* The [--infer-*] commands should not be passed by the user; we take care
     of using these commands appropriately. Fail if they are present. *)

  let () =
    List.iter stanzas ~f:(fun (stanza : stanza) ->
      List.iter stanza.flags ~f:(fun flag ->
        match flag with
        | "--depend"
        | "--raw-depend"
        | "--infer"
        | "--infer-write-query"
        | "--infer-read-reply" ->
            Loc.fail stanza.loc
              "The flag %s must not be used in a menhir stanza."
              flag
        | _ ->
            ()
      )
    )

  (* ------------------------------------------------------------------------ *)

  (* [process3 stanza] converts a Menhir stanza into a set of build rules.
     This is the three-step process where Menhir is invoked twice and OCaml
     type inference is performed in between. *)

  let process3 base (stanza : stanza) : unit =

    (* 1. A first invocation of Menhir creates a mock [.ml] file. *)

    rule_menhir
      [ As stanza.flags
      ; Deps (sources stanza.modules)
      ; As [ "--base" ; base ]
      ; A "--infer-write-query"; Target (mock_ml base)
      ];

    (* 2. The OCaml compiler performs type inference. *)

    let name = Module.Name.of_string (mock base) in

    let mock_module : Module.t =
      Module.make
        name
        ~impl:{ path = mock_ml base; syntax = OCaml }
    in

    (* The following incantation allows the mock [.ml] file to be preprocessed
       by the user-specified [ppx] rewriters. *)

    let mock_module =
      Preprocessing.pp_module_as
        (Compilation_context.preprocessing cctx)
        name
        mock_module
        ~lint:false
    in

    Module_compilation.ocamlc_i
      ~dep_graphs:(Ocamldep.rules_for_auxiliary_module cctx mock_module)
      cctx
      mock_module
      ~output:(inferred_mli base);

    (* 3. A second invocation of Menhir reads the inferred [.mli] file. *)

    rule_menhir
      [ As stanza.flags
      ; Deps (sources stanza.modules)
      ; As [ "--base" ; base ]
      ; A "--infer-read-reply"; Dep (inferred_mli base)
      ; Hidden_targets (targets base)
      ]

  (* ------------------------------------------------------------------------ *)

  (* [process3 stanza] converts a Menhir stanza into a set of build rules.
     This is a simpler one-step process where Menhir is invoked directly. *)

  let process1 base (stanza : stanza) : unit =
    rule_menhir
      [ As stanza.flags
      ; Deps (sources stanza.modules)
      ; As [ "--base" ; base ]
      ; Hidden_targets (targets base)
      ]

  (* ------------------------------------------------------------------------ *)

  (* [process stanza] converts a Menhir stanza into a set of build rules, using
     either [process3] or [process1], as appropriate. *)

  (* Because Menhir processes [--only-tokens] before the [--infer-*] commands,
     when [--only-tokens] is present, no [--infer-*] command should be used. *)

  let process (stanza : stanza) : unit =
    let base = Option.value_exn stanza.merge_into in
    let ocaml_type_inference_disabled =
      List.mem "--only-tokens" ~set:stanza.flags
    in
    if ocaml_type_inference_disabled then
      process1 base stanza
    else
      process3 base stanza

  (* ------------------------------------------------------------------------ *)

  (* The main side effect. *)

  let () =
    List.iter ~f:process stanzas

end

(* -------------------------------------------------------------------------- *)

(* The final glue. *)

let modules (stanza : Dune_file.Menhir.t) : string list =
  match stanza.merge_into with
  | Some m ->
      [m]
  | None ->
      stanza.modules

let targets (stanza : Dune_file.Menhir.t) : string list =
  let f m = [m ^ ".ml"; m ^ ".mli"] in
  List.concat_map (modules stanza) ~f

let module_names (stanza : Dune_file.Menhir.t) : Module.Name.t list =
  List.map (modules stanza) ~f:Module.Name.of_string

let gen_rules cctx stanza =
  let module R = Run (struct
    let cctx = cctx
    let stanza = stanza
  end) in
  ()
