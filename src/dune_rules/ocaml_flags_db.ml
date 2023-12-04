open Import
open Memo.O

let ocaml_flags sctx ~dir (spec : Ocaml_flags.Spec.t) =
  let* flags =
    let* expander = Super_context.expander sctx ~dir in
    let+ ocaml_flags = Super_context.env_node sctx ~dir >>= Env_node.ocaml_flags in
    Ocaml_flags.make
      ~spec
      ~default:ocaml_flags
      ~eval:(Expander.expand_and_eval_set expander)
  in
  Source_tree.is_vendored (Path.Build.drop_build_context_exn dir)
  >>= function
  | false -> Memo.return flags
  | true ->
    let+ ocaml_version =
      let+ ocaml = Super_context.context sctx |> Context.ocaml in
      ocaml.version
    in
    Ocaml_flags.with_vendored_flags ~ocaml_version flags
;;

let link_flags sctx ~dir (spec : Link_flags.Spec.t) =
  let* expander = Super_context.expander sctx ~dir in
  let+ link_flags = Super_context.env_node sctx ~dir >>= Env_node.link_flags in
  Link_flags.make ~spec ~default:link_flags ~eval:(Expander.expand_and_eval_set expander)
;;
