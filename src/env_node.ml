open Import

type t =
  { dir                 : Path.t
  ; inherit_from        : t Lazy.t option
  ; scope               : Scope.t
  ; config              : Dune_env.Stanza.t
  ; mutable ocaml_flags : Ocaml_flags.t option
  }

let make ~dir ~inherit_from ~scope ~config =
  { dir
  ; inherit_from
  ; scope
  ; config
  ; ocaml_flags = None
  }

let rec ocaml_flags node ~profile ~eval =
  match node.ocaml_flags with
  | Some x -> x
  | None ->
    let default =
      match node.inherit_from with
      | None -> Ocaml_flags.default ~profile
      | Some (lazy node) -> ocaml_flags node ~profile ~eval
    in
    let flags =
      match List.find_map node.config.rules ~f:(fun (pat, cfg) ->
        match (pat : Dune_env.Stanza.pattern), profile with
        | Any, _ -> Some cfg
        | Profile a, b -> Option.some_if (a = b) cfg)
      with
      | None -> default
      | Some cfg ->
        Ocaml_flags.make
          ~flags:cfg.flags
          ~ocamlc_flags:cfg.ocamlc_flags
          ~ocamlopt_flags:cfg.ocamlopt_flags
          ~default
          ~eval:(eval ~scope:node.scope ~dir:node.dir)
    in
    node.ocaml_flags <- Some flags;
    flags
