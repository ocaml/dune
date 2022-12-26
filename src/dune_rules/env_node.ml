open Import

module Odoc = struct
  type warnings = Dune_env.Stanza.Odoc.warnings =
    | Fatal
    | Nonfatal

  type t = { warnings : warnings }
end

module Coq = struct
  type t = string list
end

type t =
  { scope : Scope.t
  ; local_binaries : File_binding.Expanded.t list Memo.Lazy.t
  ; ocaml_flags : Ocaml_flags.t Memo.Lazy.t
  ; foreign_flags : string list Action_builder.t Foreign_language.Dict.t
  ; link_flags : Link_flags.t Memo.Lazy.t
  ; external_env : Env.t Memo.Lazy.t
  ; bin_artifacts : Artifacts.Bin.t Memo.Lazy.t
  ; inline_tests : Dune_env.Stanza.Inline_tests.t Memo.Lazy.t
  ; menhir_flags : string list Action_builder.t Memo.Lazy.t
  ; odoc : Odoc.t Memo.Lazy.t
  ; js_of_ocaml : string list Action_builder.t Js_of_ocaml.Env.t Memo.Lazy.t
  ; coq : Coq.t Action_builder.t Memo.Lazy.t
  ; format_config : Format_config.t Memo.Lazy.t
  }

let scope t = t.scope

let local_binaries t = Memo.Lazy.force t.local_binaries

let ocaml_flags t = Memo.Lazy.force t.ocaml_flags

let foreign_flags t = t.foreign_flags

let link_flags t = Memo.Lazy.force t.link_flags

let external_env t = Memo.Lazy.force t.external_env

let bin_artifacts t = Memo.Lazy.force t.bin_artifacts

let inline_tests t = Memo.Lazy.force t.inline_tests

let js_of_ocaml t = Memo.Lazy.force t.js_of_ocaml

let menhir_flags t =
  Memo.Lazy.force t.menhir_flags |> Action_builder.of_memo_join

let format_config t = Memo.Lazy.force t.format_config

let set_format_config t format_config =
  { t with format_config = Memo.Lazy.of_val format_config }

let odoc t = Memo.Lazy.force t.odoc

let coq t = Memo.Lazy.force t.coq

let expand_str_lazy expander sw =
  match String_with_vars.text_only sw with
  | Some s -> Memo.return s
  | None ->
    let open Memo.O in
    let* expander = Memo.Lazy.force expander in
    Expander.No_deps.expand_str expander sw

let make ~dir ~inherit_from ~scope ~config_stanza ~profile ~expander
    ~expander_for_artifacts ~default_context_flags ~default_env
    ~default_bin_artifacts ~default_cxx_link_flags =
  let open Memo.O in
  let config = Dune_env.Stanza.find config_stanza ~profile in
  let inherited ~field ~root extend =
    Memo.lazy_ (fun () ->
        (match inherit_from with
        | None -> Memo.return root
        | Some t -> Memo.Lazy.force t >>= field)
        >>= extend)
  in
  let inherited_if_absent ~field ~root f_absent =
    Memo.lazy_ (fun () ->
        match root with
        | None -> (
          match inherit_from with
          | None -> f_absent None
          | Some t ->
            let* field = Memo.Lazy.force t >>= field in
            f_absent (Some field))
        | Some x -> Memo.return x)
  in
  let local_binaries =
    Memo.lazy_ (fun () ->
        Memo.sequential_map config.binaries
          ~f:
            (File_binding.Unexpanded.expand ~dir
               ~f:(expand_str_lazy expander_for_artifacts)))
  in
  let external_env =
    inherited ~field:external_env ~root:default_env (fun env ->
        let env, have_binaries =
          (Env.extend_env env config.env_vars, List.is_non_empty config.binaries)
        in
        Memo.return
        @@
        if have_binaries then
          let dir = Artifacts.Bin.local_bin dir |> Path.build in
          Env_path.cons env ~dir
        else env)
  in
  let bin_artifacts =
    inherited ~field:bin_artifacts ~root:default_bin_artifacts (fun binaries ->
        let+ local_binaries = Memo.Lazy.force local_binaries in
        Artifacts.Bin.add_binaries binaries ~dir local_binaries)
  in
  let ocaml_flags =
    let default_ocaml_flags =
      let project = Scope.project scope in
      let dune_version = Dune_project.dune_version project in
      Ocaml_flags.default ~profile ~dune_version
    in
    inherited ~field:ocaml_flags ~root:default_ocaml_flags (fun flags ->
        let+ expander = Memo.Lazy.force expander in
        let expander = Expander.set_dir expander ~dir in
        Ocaml_flags.make ~spec:config.flags ~default:flags
          ~eval:(Expander.expand_and_eval_set expander))
  in
  let inline_tests =
    match config with
    | { inline_tests = Some s; _ } -> Memo.Lazy.of_val s
    | { inline_tests = None; _ } ->
      inherited ~field:inline_tests Memo.return
        ~root:
          (if Profile.is_inline_test profile then
           Dune_env.Stanza.Inline_tests.Enabled
          else Disabled)
  in
  let js_of_ocaml =
    inherited
      ~field:(fun t -> js_of_ocaml t)
      ~root:Js_of_ocaml.Env.(map ~f:Action_builder.return (default ~profile))
      (fun (jsoo : _ Action_builder.t Js_of_ocaml.Env.t) ->
        let local = config.js_of_ocaml in
        let+ expander = Memo.Lazy.force expander in
        let expander = Expander.set_dir expander ~dir in
        let pick ~first ~second =
          match first with
          | None -> second
          | Some _ as x -> x
        in
        { Js_of_ocaml.Env.compilation_mode =
            pick ~first:local.compilation_mode ~second:jsoo.compilation_mode
        ; runtest_alias =
            pick ~first:local.runtest_alias ~second:jsoo.runtest_alias
        ; flags =
            Js_of_ocaml.Flags.make ~spec:local.flags ~default:jsoo.flags
              ~eval:(Expander.expand_and_eval_set expander)
        })
  in
  let foreign_flags lang =
    let field t =
      Memo.return (Foreign_language.Dict.get t.foreign_flags lang)
    in
    Action_builder.of_memo_join
      (Memo.Lazy.force
         (inherited ~field
            ~root:(Foreign_language.Dict.get default_context_flags lang)
            (fun flags ->
              let+ expander = Memo.Lazy.force expander in
              let expander = Expander.set_dir expander ~dir in
              let f = Foreign_language.Dict.get config.foreign_flags lang in
              Expander.expand_and_eval_set expander f ~standard:flags)))
  in
  let foreign_flags =
    Foreign_language.Dict.make ~c:(foreign_flags C) ~cxx:(foreign_flags Cxx)
  in
  let link_flags =
    let default_link_flags = Link_flags.default ~default_cxx_link_flags in
    inherited ~field:link_flags ~root:default_link_flags (fun link_flags ->
        let+ expander = Memo.Lazy.force expander in
        let expander = Expander.set_dir expander ~dir in
        Link_flags.make ~spec:config.link_flags ~default:link_flags
          ~eval:(Expander.expand_and_eval_set expander))
  in
  let menhir_flags =
    inherited
      ~field:(fun t -> Memo.return (menhir_flags t))
      ~root:(Action_builder.return [])
      (fun flags ->
        let+ expander = Memo.Lazy.force expander in
        let expander = Expander.set_dir expander ~dir in
        Expander.expand_and_eval_set expander config.menhir_flags
          ~standard:flags)
  in
  let odoc =
    let open Odoc in
    let root =
      (* DUNE3: Enable for dev profile in the future *)
      { warnings = Nonfatal }
    in
    inherited ~field:odoc ~root (fun { warnings } ->
        Memo.return
          { warnings = Option.value config.odoc.warnings ~default:warnings })
  in
  let default_coq_flags = Action_builder.return [ "-q" ] in
  let coq : Coq.t Action_builder.t Memo.Lazy.t =
    inherited ~field:coq ~root:default_coq_flags (fun flags ->
        let+ expander = Memo.Lazy.force expander in
        let expander = Expander.set_dir expander ~dir in
        let standard = flags in
        Expander.expand_and_eval_set expander config.coq ~standard)
  in
  let format_config =
    inherited_if_absent ~field:format_config ~root:config.format_config
      (function
      | None ->
        Code_error.raise
          "format config should always have a default value taken from the \
           project root"
          []
      | Some x -> Memo.return x)
  in
  { scope
  ; ocaml_flags
  ; foreign_flags
  ; link_flags
  ; external_env
  ; bin_artifacts
  ; local_binaries
  ; inline_tests
  ; js_of_ocaml
  ; menhir_flags
  ; odoc
  ; coq
  ; format_config
  }
