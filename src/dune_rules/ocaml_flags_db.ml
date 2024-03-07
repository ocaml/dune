open Import
open Memo.O

let ocaml_flags_env =
  let f =
    Env_stanza_db_flags.flags
      ~name:"ocaml-flags-env"
      ~root:(fun ctx project ->
        let+ profile = Context.DB.get ctx >>| Context.profile in
        let dune_version = Dune_project.dune_version project in
        Ocaml_flags.default ~profile ~dune_version)
      ~f:(fun ~parent expander (env : Dune_env.config) ->
        let+ parent = parent in
        Ocaml_flags.make
          ~spec:env.flags
          ~default:parent
          ~eval:(Expander.expand_and_eval_set expander))
  in
  fun ~dir ->
    let* () = Memo.return () in
    (Staged.unstage f) dir
;;

let ocaml_flags sctx ~dir (spec : Ocaml_flags.Spec.t) =
  let* flags =
    let* expander = Super_context.expander sctx ~dir in
    let+ ocaml_flags = ocaml_flags_env ~dir in
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

let link_env =
  let f =
    Env_stanza_db_flags.flags
      ~name:"link-env"
      ~root:(fun ctx _ ->
        let default_cxx_link_flags =
          Cxx_flags.get_flags ~for_:Link (Build_context.create ~name:ctx)
        in
        Link_flags.default ~default_cxx_link_flags |> Memo.return)
      ~f:(fun ~parent expander (env : Dune_env.config) ->
        let+ parent = parent in
        Link_flags.make
          ~spec:env.link_flags
          ~default:parent
          ~eval:(Expander.expand_and_eval_set expander))
  in
  fun ~dir ->
    let* () = Memo.return () in
    (Staged.unstage f) dir
;;

let link_flags sctx ~dir (spec : Link_flags.Spec.t) =
  let* expander = Super_context.expander sctx ~dir in
  let+ link_flags = link_env ~dir in
  Link_flags.make ~spec ~default:link_flags ~eval:(Expander.expand_and_eval_set expander)
;;
