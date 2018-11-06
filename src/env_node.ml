open Stdune

type t =
  { dir                 : Path.t
  ; inherit_from        : t Lazy.t option
  ; scope               : Scope.t
  ; config              : Dune_env.Stanza.t
  ; mutable ocaml_flags : Ocaml_flags.t option
  ; mutable external_   : Env.t option
  }

let scope t = t.scope

let make ~dir ~inherit_from ~scope ~config ~env =
  { dir
  ; inherit_from
  ; scope
  ; config
  ; ocaml_flags = None
  ; external_ = env
  }

let rec external_ t ~profile ~default =
  match t.external_ with
  | Some x -> x
  | None ->
    let default =
      match t.inherit_from with
      | None -> default
      | Some (lazy t) -> external_ t ~default ~profile
    in
    let env =
      match Dune_env.Stanza.find t.config ~profile with
      | None -> default
      | Some cfg -> Env.extend_env default cfg.env_vars
    in
    t.external_ <- Some env;
    env

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
      match Dune_env.Stanza.find t.config ~profile with
      | None -> default
      | Some cfg ->
        let expander = Expander.set_dir expander ~dir:t.dir in
        Ocaml_flags.make
          ~flags:cfg.flags
          ~ocamlc_flags:cfg.ocamlc_flags
          ~ocamlopt_flags:cfg.ocamlopt_flags
          ~default
          ~eval:(Expander.expand_and_eval_set expander)
    in
    t.ocaml_flags <- Some flags;
    flags
