open Import

(*
TODO:

1. Call coqmod
2. Parse output of coqmod (using csexp)
3. Resolve the 5 kinds of dependencies

*)

(* Action that produces deps using coqmod  *)
let action =
  let module Spec = struct
    type ('path, 'target) t = 'path * 'target

    let name = "coqmod"

    let version = 1

    let bimap (src, dst) f g = (f src, g dst)

    let is_useful_to ~distribute:_ ~memoize = memoize

    let encode (src, dst) path target : Dune_lang.t =
      List [ Dune_lang.atom_or_quoted_string name; path src; target dst ]

    (* Call coqmod here *)
    let action (src, dst) ~ectx:_ ~eenv:_ =
      let deps = Io.Untracked.with_lexbuf_from_file src ~f:Coqmod.lexbuf in
      Io.with_file_out (Path.build dst) ~f:(fun ch ->
          Csexp.to_channel ch (Coqmod.to_csexp deps));
      Fiber.return ()
  end in
  fun (src : Path.t) (dst : Path.Build.t) ->
    let module M :
      Action.Ext.Instance with type path = Path.t and type target = Path.Build.t =
    struct
      type path = Path.t

      type target = Path.Build.t

      module Spec = Spec

      let v = (src, dst)
    end in
    Action.Extension (module M)

let deps_of coq_module =
  let open Action_builder.O in
  let file = Coq_module.dep_file coq_module in
  let+ contents = Action_builder.contents (Path.build file) in
  match Csexp.parse_string contents with
  | Error _ -> failwith "TODO"
  | Ok s -> Coqmod.of_csexp s

let add_rule sctx coq_module =
  let rule =
    let dst = Coq_module.dep_file coq_module in
    (let open Action_builder.O in
    let src = Path.build (Coq_module.source coq_module) in
    let+ () = Action_builder.path src in
    let dst = Coq_module.dep_file coq_module in
    Action.Full.make (action src dst))
    |> Action_builder.with_file_targets ~file_targets:[ dst ]
  in
  Super_context.add_rule sctx ~dir:(Coq_module.obj_dir coq_module) rule
