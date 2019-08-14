open Stdune

type t =
  { dir : Path.Build.t
  ; inherit_from : t Lazy.t option
  ; scope : Scope.t
  ; config : Dune_env.Stanza.t
  ; mutable local_binaries : File_binding.Expanded.t list option
  ; mutable ocaml_flags : Ocaml_flags.t option
  ; mutable c_flags : (unit, string list) Build.t C.Kind.Dict.t option
  ; mutable external_ : Env.t option
  ; mutable bin_artifacts : Artifacts.Bin.t option
  ; mutable inline_tests : Dune_env.Stanza.Inline_tests.t option
  }

let scope t = t.scope

let make ~dir ~inherit_from ~scope ~config =
  { dir
  ; inherit_from
  ; scope
  ; config
  ; ocaml_flags = None
  ; c_flags = None
  ; external_ = None
  ; bin_artifacts = None
  ; local_binaries = None
  ; inline_tests = None
  }

let find_config t ~profile = Dune_env.Stanza.find t.config ~profile

let rec local_binaries t ~profile ~expander =
  match t.local_binaries with
  | Some x ->
    x
  | None ->
    let default =
      match t.inherit_from with
      | None ->
        []
      | Some (lazy t) ->
        local_binaries t ~profile ~expander
    in
    let local_binaries =
      default
      @ List.map (find_config t ~profile).binaries
        ~f:
          (File_binding.Unexpanded.expand ~dir:t.dir ~f:(fun template ->
            Expander.expand expander ~mode:Single ~template
            |> Value.to_string ~dir:(Path.build t.dir)))
    in
    t.local_binaries <- Some local_binaries;
    local_binaries

let rec external_ t ~profile ~default =
  match t.external_ with
  | Some x ->
    x
  | None ->
    let default =
      match t.inherit_from with
      | None ->
        default
      | Some (lazy t) ->
        external_ t ~default ~profile
    in
    let env, have_binaries =
      let cfg = find_config t ~profile in
      (Env.extend_env default cfg.env_vars, not (List.is_empty cfg.binaries))
    in
    let env =
      if have_binaries then
        let dir = Utils.local_bin t.dir |> Path.build in
        Env.cons_path env ~dir
      else
        env
    in
    t.external_ <- Some env;
    env

let rec bin_artifacts t ~profile ~default ~expander =
  match t.bin_artifacts with
  | Some x ->
    x
  | None ->
    let default =
      match t.inherit_from with
      | None ->
        default
      | Some (lazy t) ->
        bin_artifacts t ~default ~profile ~expander
    in
    let bin_artifacts =
      local_binaries t ~profile ~expander
      |> Artifacts.Bin.add_binaries default ~dir:t.dir
    in
    t.bin_artifacts <- Some bin_artifacts;
    bin_artifacts

let rec ocaml_flags t ~profile ~expander =
  match t.ocaml_flags with
  | Some x ->
    x
  | None ->
    let default =
      match t.inherit_from with
      | None ->
        let project = Scope.project t.scope in
        let dune_version = Dune_project.dune_version project in
        Ocaml_flags.default ~profile ~dune_version
      | Some (lazy t) ->
        ocaml_flags t ~profile ~expander
    in
    let flags =
      let cfg = find_config t ~profile in
      let expander = Expander.set_dir expander ~dir:t.dir in
      Ocaml_flags.make ~spec:cfg.flags ~default
        ~eval:(Expander.expand_and_eval_set expander)
    in
    t.ocaml_flags <- Some flags;
    flags

let rec inline_tests t ~profile =
  match t.inline_tests with
  | Some x ->
    x
  | None ->
    let state : Dune_env.Stanza.Inline_tests.t =
      match find_config t ~profile with
      | { inline_tests = None; _ } -> (
        match t.inherit_from with
        | None ->
          if Profile.is_inline_test profile then
            Enabled
          else
            Disabled
        | Some (lazy t) ->
          inline_tests t ~profile )
      | { inline_tests = Some s; _ } ->
        s
    in
    t.inline_tests <- Some state;
    state

let rec c_flags t ~profile ~expander ~default_context_flags =
  match t.c_flags with
  | Some x ->
    x
  | None ->
    let default =
      match t.inherit_from with
      | None ->
        C.Kind.Dict.map ~f:Build.return default_context_flags
      | Some (lazy t) ->
        c_flags t ~profile ~expander ~default_context_flags
    in
    let flags =
      let cfg = find_config t ~profile in
      let expander = Expander.set_dir expander ~dir:t.dir in
      C.Kind.Dict.mapi cfg.c_flags ~f:(fun ~kind f ->
        let default = C.Kind.Dict.get default kind in
        Expander.expand_and_eval_set expander f ~standard:default)
    in
    t.c_flags <- Some flags;
    flags
