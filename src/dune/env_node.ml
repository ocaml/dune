open Stdune

module Odoc = struct
  type warnings = Dune_env.Stanza.Odoc.warnings =
    | Fatal
    | Nonfatal

  type t = { warnings : warnings }
end

type t =
  { dir : Path.Build.t
  ; inherit_from : t Memo.Lazy.t option
  ; scope : Scope.t
  ; config : Dune_env.Stanza.t
  ; profile : Profile.t
  ; expander : Expander.t
  ; local_binaries : File_binding.Expanded.t list Memo.Lazy.t
  ; ocaml_flags : Ocaml_flags.t Memo.Lazy.t
  ; foreign_flags : string list Build.t Foreign.Language.Dict.t Memo.Lazy.t
  ; external_ : Env.t Memo.Lazy.t
  ; bin_artifacts : Artifacts.Bin.t Memo.Lazy.t
  ; inline_tests : Dune_env.Stanza.Inline_tests.t Memo.Lazy.t
  ; menhir_flags : string list Build.t Memo.Lazy.t
  ; odoc : Odoc.t Memo.Lazy.t
  }

let scope t = t.scope

let local_binaries t = Memo.Lazy.force t.local_binaries

let ocaml_flags t = Memo.Lazy.force t.ocaml_flags

let foreign_flags t = Memo.Lazy.force t.foreign_flags

let external_ t = Memo.Lazy.force t.external_

let bin_artifacts t = Memo.Lazy.force t.bin_artifacts

let inline_tests t = Memo.Lazy.force t.inline_tests

let menhir_flags t = Memo.Lazy.force t.menhir_flags

let local_binaries_impl ~inherit_from ~dir ~config ~profile ~expander =
  let default =
    match inherit_from with
    | None -> []
    | Some t -> local_binaries (Memo.Lazy.force t)
  in
  default
  @ List.map (Dune_env.Stanza.find config ~profile).binaries
      ~f:
        (File_binding.Unexpanded.expand ~dir ~f:(fun template ->
             Expander.expand expander ~mode:Single ~template
             |> Value.to_string ~dir:(Path.build dir)))

let external_impl ~inherit_from ~dir ~config ~profile ~default_env =
  let default =
    match inherit_from with
    | None -> default_env
    | Some t -> external_ (Memo.Lazy.force t)
  in
  let env, have_binaries =
    let cfg = Dune_env.Stanza.find config ~profile in
    (Env.extend_env default cfg.env_vars, List.is_non_empty cfg.binaries)
  in
  if have_binaries then
    let dir = Utils.local_bin dir |> Path.build in
    Env.cons_path env ~dir
  else
    env

let bin_artifacts_impl ~inherit_from ~dir ~config ~profile ~expander
    ~default_bin_artifacts =
  let default =
    match inherit_from with
    | None -> default_bin_artifacts
    | Some t -> bin_artifacts (Memo.Lazy.force t)
  in
  local_binaries_impl ~inherit_from ~dir ~config ~profile ~expander
  |> Artifacts.Bin.add_binaries default ~dir

let ocaml_flags_impl ~inherit_from ~dir ~config ~scope ~profile ~expander =
  let default =
    match inherit_from with
    | None ->
      let project = Scope.project scope in
      let dune_version = Dune_project.dune_version project in
      Ocaml_flags.default ~profile ~dune_version
    | Some t -> ocaml_flags (Memo.Lazy.force t)
  in
  let cfg = Dune_env.Stanza.find config ~profile in
  let expander = Expander.set_dir expander ~dir in
  Ocaml_flags.make ~spec:cfg.flags ~default
    ~eval:(Expander.expand_and_eval_set expander)

let inline_tests_impl ~inherit_from ~config ~profile =
  match Dune_env.Stanza.find config ~profile with
  | { inline_tests = None; _ } -> (
    match inherit_from with
    | None ->
      if Profile.is_inline_test profile then
        Dune_env.Stanza.Inline_tests.Enabled
      else
        Disabled
    | Some t -> inline_tests (Memo.Lazy.force t) )
  | { inline_tests = Some s; _ } -> s

let foreign_flags_impl ~inherit_from ~dir ~config ~profile ~expander
    ~default_context_flags =
  let default =
    match inherit_from with
    | None -> Foreign.Language.Dict.map ~f:Build.return default_context_flags
    | Some t -> foreign_flags (Memo.Lazy.force t)
  in
  let cfg = Dune_env.Stanza.find config ~profile in
  let expander = Expander.set_dir expander ~dir in
  Foreign.Language.Dict.mapi cfg.foreign_flags ~f:(fun ~language f ->
      let default = Foreign.Language.Dict.get default language in
      Expander.expand_and_eval_set expander f ~standard:default)

let menhir_flags_impl ~dir ~inherit_from ~config ~profile ~expander =
  let default =
    match inherit_from with
    | None -> Build.return []
    | Some t -> menhir_flags (Memo.Lazy.force t)
  in
  let cfg = Dune_env.Stanza.find config ~profile in
  let expander = Expander.set_dir expander ~dir in
  Expander.expand_and_eval_set expander cfg.menhir_flags ~standard:default

let make ~dir ~inherit_from ~scope ~config ~profile ~expander
    ~default_context_flags ~default_env ~default_bin_artifacts =
  { dir
  ; inherit_from
  ; scope
  ; config
  ; profile
  ; expander
  ; ocaml_flags =
      Memo.lazy_ (fun () ->
          ocaml_flags_impl ~inherit_from ~dir ~config ~scope ~profile ~expander)
  ; foreign_flags =
      Memo.lazy_ (fun () ->
          foreign_flags_impl ~inherit_from ~dir ~config ~profile ~expander
            ~default_context_flags)
  ; external_ =
      Memo.lazy_ (fun () ->
          external_impl ~inherit_from ~dir ~config ~profile ~default_env)
  ; bin_artifacts =
      Memo.lazy_ (fun () ->
          bin_artifacts_impl ~inherit_from ~dir ~config ~profile ~expander
            ~default_bin_artifacts)
  ; local_binaries =
      Memo.lazy_ (fun () ->
          local_binaries_impl ~inherit_from ~dir ~config ~profile ~expander)
  ; inline_tests =
      Memo.lazy_ (fun () -> inline_tests_impl ~inherit_from ~config ~profile)
  ; menhir_flags =
      Memo.lazy_ (fun () ->
          menhir_flags_impl ~inherit_from ~dir ~config ~profile ~expander)
  ; odoc
  }

let rec odoc t ~profile =
  match t.odoc with
  | Some x -> x
  | None ->
    let open Odoc in
    let default =
      (* DUNE3: Enable for dev profile in the future *)
      { warnings = Nonfatal }
    in
    let inherited =
      match t.inherit_from with
      | None -> default
      | Some (lazy t) -> odoc t ~profile
    in
    let conf =
      let c = (find_config t ~profile).odoc in
      { warnings = Option.value c.warnings ~default:inherited.warnings }
    in
    t.odoc <- Some conf;
    conf
