open! Dune_engine
open Import

module Odoc = struct
  type warnings = Dune_env.Stanza.Odoc.warnings =
    | Fatal
    | Nonfatal

  type t = { warnings : warnings }
end

module Coq = struct
  type t = Ordered_set_lang.Unexpanded.t
end

type t =
  { scope : Scope.t
  ; local_binaries : File_binding.Expanded.t list Memo.Lazy.Async.t
  ; ocaml_flags : Ocaml_flags.t Memo.Lazy.Async.t
  ; foreign_flags :
      string list Action_builder.t Foreign_language.Dict.t Memo.Lazy.Async.t
  ; external_env : Env.t Memo.Lazy.Async.t
  ; bin_artifacts : Artifacts.Bin.t Memo.Lazy.Async.t
  ; inline_tests : Dune_env.Stanza.Inline_tests.t Memo.Lazy.Async.t
  ; menhir_flags : string list Action_builder.t Memo.Lazy.Async.t
  ; odoc : Odoc.t Memo.Lazy.Async.t
  ; coq : Coq.t Memo.Lazy.Async.t
  ; format_config : Format_config.t Memo.Lazy.Async.t
  }

let scope t = t.scope

let local_binaries t = Memo.Lazy.Async.force t.local_binaries

let ocaml_flags t = Memo.Lazy.Async.force t.ocaml_flags

let foreign_flags t = Memo.Lazy.Async.force t.foreign_flags

let external_env t = Memo.Lazy.Async.force t.external_env

let bin_artifacts t = Memo.Lazy.Async.force t.bin_artifacts

let inline_tests t = Memo.Lazy.Async.force t.inline_tests

let menhir_flags t = Memo.Lazy.Async.force t.menhir_flags

let format_config t = Memo.Lazy.Async.force t.format_config

let set_format_config t format_config =
  { t with format_config = Memo.Lazy.Async.of_val format_config }

let odoc t = Memo.Lazy.Async.force t.odoc

let coq t = Memo.Lazy.Async.force t.coq

let make ~dir ~inherit_from ~scope ~config_stanza ~profile ~expander
    ~expander_for_artifacts ~default_context_flags ~default_env
    ~default_bin_artifacts =
  let open Memo.Build.O in
  let config = Dune_env.Stanza.find config_stanza ~profile in
  let inherited ~field ~root extend =
    Memo.lazy_async (fun () ->
        (match inherit_from with
        | None -> root
        | Some t -> Memo.Lazy.Async.force t >>= field)
        >>= extend)
  in
  let inherited_if_absent ~field ~root f_absent =
    Memo.lazy_async (fun () ->
        match root with
        | None -> (
          match inherit_from with
          | None -> f_absent None
          | Some t ->
            let* t = Memo.Lazy.Async.force t in
            let* field = field t in
            f_absent (Some field))
        | Some x -> Memo.Build.return x)
  in
  let local_binaries =
    inherited ~field:local_binaries ~root:(Memo.Build.return [])
      (fun binaries ->
        let+ expander_for_artifacts =
          Memo.Lazy.Async.force expander_for_artifacts
        in
        binaries
        @ List.map config.binaries
            ~f:
              (File_binding.Unexpanded.expand ~dir ~f:(fun template ->
                   Expander.Static.expand_str expander_for_artifacts template)))
  in
  let external_env =
    inherited ~field:external_env ~root:default_env (fun env ->
        let env, have_binaries =
          (Env.extend_env env config.env_vars, List.is_non_empty config.binaries)
        in
        Memo.Build.return
          (if have_binaries then
            let dir = Utils.local_bin dir |> Path.build in
            Env.cons_path env ~dir
          else
            env))
  in
  let bin_artifacts =
    inherited ~field:bin_artifacts ~root:default_bin_artifacts (fun binaries ->
        let+ local_binaries = Memo.Lazy.Async.force local_binaries in
        Artifacts.Bin.add_binaries binaries ~dir local_binaries)
  in
  let ocaml_flags =
    let default_ocaml_flags =
      let project = Scope.project scope in
      let dune_version = Dune_project.dune_version project in
      Ocaml_flags.default ~profile ~dune_version
    in
    inherited ~field:ocaml_flags ~root:(Memo.Build.return default_ocaml_flags)
      (fun flags ->
        let+ expander = Memo.Lazy.Async.force expander in
        let expander = Expander.set_dir expander ~dir in
        Ocaml_flags.make ~spec:config.flags ~default:flags
          ~eval:(Expander.expand_and_eval_set expander))
  in
  let inline_tests =
    match config with
    | { inline_tests = Some s; _ } -> Memo.Lazy.Async.of_val s
    | { inline_tests = None; _ } ->
      inherited ~field:inline_tests Memo.Build.return
        ~root:
          (Memo.Build.return
             (if Profile.is_inline_test profile then
               Dune_env.Stanza.Inline_tests.Enabled
             else
               Disabled))
  in
  let foreign_flags =
    inherited ~field:foreign_flags ~root:default_context_flags (fun flags ->
        let+ expander = Memo.Lazy.Async.force expander in
        let expander = Expander.set_dir expander ~dir in
        Foreign_language.Dict.mapi config.foreign_flags ~f:(fun ~language f ->
            let standard = Foreign_language.Dict.get flags language in
            Expander.expand_and_eval_set expander f ~standard))
  in
  let menhir_flags =
    inherited ~field:menhir_flags
      ~root:(Memo.Build.return (Action_builder.return []))
      (fun flags ->
        let+ expander = Memo.Lazy.Async.force expander in
        let expander = Expander.set_dir expander ~dir in
        Expander.expand_and_eval_set expander config.menhir_flags
          ~standard:flags)
  in
  let odoc =
    let open Odoc in
    let root =
      (* DUNE3: Enable for dev profile in the future *)
      Memo.Build.return { warnings = Nonfatal }
    in
    inherited ~field:odoc ~root (fun { warnings } ->
        Memo.Build.return
          { warnings = Option.value config.odoc.warnings ~default:warnings })
  in
  let coq =
    inherited ~field:coq ~root:(Memo.Build.return config.coq) Memo.Build.return
  in
  let format_config =
    inherited_if_absent ~field:format_config ~root:config.format_config
      (function
      | None ->
        Code_error.raise
          "format config should always have a default value taken from the \
           project root"
          []
      | Some x -> Memo.Build.return x)
  in
  { scope
  ; ocaml_flags
  ; foreign_flags
  ; external_env
  ; bin_artifacts
  ; local_binaries
  ; inline_tests
  ; menhir_flags
  ; odoc
  ; coq
  ; format_config
  }
