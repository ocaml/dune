open Import
open Memo.O

let setup_of_ocaml_rules ~sctx ~dir ({ loc; modules } : Coq_of_ocaml_stanza.t) =
  let* coq_of_ocaml =
    Super_context.resolve_program ~dir sctx "coq-of-ocaml" ~loc:(Some loc)
      ~hint:"opam install coq-of-ocaml"
  in
  let coq_of_ocaml_rule m =
    let source = Path.build (Path.Build.relative dir (m ^ ".ml")) in
    let target = Path.Build.relative dir (m ^ ".v") in
    let args = [ Command.Args.Dep source; A "-output"; Target target ] in
    Command.run ~dir:(Path.build dir) coq_of_ocaml args
  in
  List.rev_map ~f:coq_of_ocaml_rule modules
  |> Super_context.add_rules ~loc ~dir sctx
