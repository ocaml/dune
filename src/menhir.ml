open Import
open Build.O
open! No_io

module SC = Super_context

(* -------------------------------------------------------------------------- *)

(* This signature describes the input of the functor [Run], which follows. *)

type stanza =
  Dune_file.Menhir.t

module type PARAMS = sig

  (* [sctx] is the super context. It stores all the information about the
     current build context. The current compiler can be obtained via
     [(SC.context sctx).ocamlc]. *)

  val sctx: SC.t

  (* [dir] is the directory inside [_build/<context>/...] where the build
     happens. If the [(menhir ...)] stanza appears in [src/jbuild], then [dir]
     is of the form [_build/<context>/src], e.g., [_build/default/src]. *)

  val dir: Path.t

  (* [scope] represents the scope this stanza is part of. Dune allows building
     multiple projects at once and splits the source tree into one scope per
     project. *)

  val scope: Scope.t

  (* [stanza] is the [(menhir ...)] stanza, as found in the [jbuild] file. *)

  val stanza: stanza

end

(* -------------------------------------------------------------------------- *)

(* This functor is where [(menhir ...)] stanzas are desugared. *)

module Run (P : PARAMS) = struct

  open P

  let context =
    SC.context sctx

  (* If [m] is a (short) module name, such as "myparser", then [source dir m]
     is the corresponding source file, and [targets dir m] is the list of
     targets that Menhir must build. *)

  let source m =
    Path.relative dir (m ^ ".mly")

  let targets m =
    List.map ~f:(Path.relative dir) [m ^ ".ml"; m ^ ".mli"]

  let sources ms =
    List.map ~f:source ms

  (* Expand special variables, such as %{workspace_root}, in the stanza's
     flags. *)

  let flags =
    SC.expand_and_eval_set
      sctx ~scope ~dir stanza.flags ~standard:(Build.return [])

  (* Find the menhir binary. *)

  let menhir_binary =
    SC.resolve_program sctx "menhir" ~loc:None ~hint:"try: opam install menhir"

  (* [menhir args] generates a Menhir command line (a build action). *)

  type args =
    string list Arg_spec.t list

  let menhir (args : args) =
    flags >>> Build.run menhir_binary ~dir ~context args

  let rule : (unit, Action.t) Build.t -> unit =
    SC.add_rule sctx ~mode:stanza.mode ~loc:stanza.loc

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

  (* [process stanza] converts a Menhir stanza into a rule, which it installs. *)

  (* Reminder (from arg_spec.mli):

     [Deps]           is for command line arguments that are dependencies.
     [As]             is for command line arguments
                      that are neither dependencies nor targets.
     [Hidden_targets] is for targets that are *not* command line arguments.

     [Dyn (fun flags -> As flags)]
                      indicates that any flags that appear in the stanza
                      must be transmitted to Menhir.

   *)

  let process (stanza : stanza) =
    let base : string = Option.value_exn stanza.merge_into in
    let args : args =
      [ Dyn (fun flags -> As flags)
      ; Deps (sources stanza.modules)
      ; As [ "--base" ; base ]
      ; Hidden_targets (targets base)
      ]
    in
    rule (menhir args)

  (* The main side effect. *)

  let () =
    List.iter ~f:process stanzas

end

(* -------------------------------------------------------------------------- *)

(* The final glue. *)

let targets (stanza : Dune_file.Menhir.t) =
  let f m = [m ^ ".ml"; m ^ ".mli"] in
  match stanza.merge_into with
  | Some m -> f m
  | None -> List.concat_map stanza.modules ~f

let module_names (stanza : Dune_file.Menhir.t) =
  match stanza.merge_into with
  | Some m -> [Module.Name.of_string m]
  | None -> List.map stanza.modules ~f:Module.Name.of_string

let gen_rules cctx stanza =
  let module R = Run(struct
    let sctx = Compilation_context.super_context cctx
    let dir = Compilation_context.dir cctx
    let scope = Compilation_context.scope cctx
    let stanza = stanza
  end) in
  ()
