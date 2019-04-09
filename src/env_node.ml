open Stdune

type t =
  { dir                   : Path.t
  ; inherit_from          : t Lazy.t option
  ; scope                 : Scope.t
  ; config                : Dune_env.Stanza.t option
  ; mutable local_binaries : File_binding.Expanded.t list option
  ; mutable ocaml_flags   : Ocaml_flags.t option
  ; mutable c_flags       : (unit, string list) Build.t C.Kind.Dict.t option
  ; mutable external_     : Env.t option
  ; mutable artifacts     : Artifacts.t option
  }

let scope t = t.scope

let make ~dir ~inherit_from ~scope ~config ~env =
  { dir
  ; inherit_from
  ; scope
  ; config
  ; ocaml_flags = None
  ; c_flags = None
  ; external_ = env
  ; artifacts = None
  ; local_binaries = None
  }

let find_config t ~profile =
  let open Option.O in
  t.config >>= Dune_env.Stanza.find ~profile

let rec local_binaries t ~profile ~expander =
  match t.local_binaries with
  | Some x -> x
  | None ->
    let default =
      match t.inherit_from with
      | None -> []
      | Some (lazy t) -> local_binaries t ~profile ~expander
    in
    let local_binaries =
      match find_config t ~profile with
      | None -> default
      | Some cfg ->
        default @
        List.map cfg.binaries
          ~f:(File_binding.Unexpanded.expand ~dir:t.dir ~f:(fun template ->
            Expander.expand expander ~mode:Single ~template
            |> Value.to_string ~dir:t.dir))
    in
    t.local_binaries <- Some local_binaries;
    local_binaries

let rec external_ t ~profile ~default =
  match t.external_ with
  | Some x -> x
  | None ->
    let default =
      match t.inherit_from with
      | None -> default
      | Some (lazy t) -> external_ t ~default ~profile
    in
    let (env, have_binaries) =
      match find_config t ~profile with
      | None -> (default, false)
      | Some cfg ->
        ( Env.extend_env default cfg.env_vars
        , not (List.is_empty cfg.binaries)
        )
    in
    let env =
      if have_binaries then
        Env.cons_path env ~dir:(Utils.local_bin t.dir)
      else
        env
    in
    t.external_ <- Some env;
    env

let rec artifacts t ~profile ~default ~expander =
  match t.artifacts with
  | Some x -> x
  | None ->
    let default =
      match t.inherit_from with
      | None -> default
      | Some (lazy t) -> artifacts t ~default ~profile ~expander
    in
    let artifacts =
      local_binaries t ~profile ~expander
      |> Artifacts.add_binaries default ~dir:t.dir
    in
    t.artifacts <- Some artifacts;
    artifacts

let rec ocaml_flags t ~profile ~expander =
  match t.ocaml_flags with
  | Some x -> x
  | None ->
    let default =
      match t.inherit_from with
      | None -> Ocaml_flags.default ~profile
      | Some (lazy t) -> ocaml_flags t ~profile ~expander
    in
    let flags =
      match find_config t ~profile with
      | None -> default
      | Some cfg ->
        let expander = Expander.set_dir expander ~dir:t.dir in
        Ocaml_flags.make
          ~spec:cfg.flags
          ~default
          ~eval:(Expander.expand_and_eval_set expander)
    in
    t.ocaml_flags <- Some flags;
    flags

let rec c_flags t ~profile ~expander ~default_context_flags =
  match t.c_flags with
  | Some x -> x
  | None ->
    let default =
      match t.inherit_from with
      | None -> C.Kind.Dict.map ~f:Build.return default_context_flags
      | Some (lazy t) -> c_flags t ~profile ~expander ~default_context_flags
    in
    let flags =
      match find_config t ~profile with
      | None -> default
      | Some cfg ->
        let expander = Expander.set_dir expander ~dir:t.dir in
        let eval = Expander.expand_and_eval_set expander in
        C.Kind.Dict.mapi cfg.c_flags ~f:(fun ~kind f ->
          if Ordered_set_lang.Unexpanded.has_special_forms f then
            let default = C.Kind.Dict.get default kind in
            eval f ~standard:default
          else
            eval f ~standard:(Build.return []))
    in
    t.c_flags <- Some flags;
    flags
