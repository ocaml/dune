open Stdune

type t = { dune_version : Dune_lang.Syntax.Version.t }

let make ~dune_version = { dune_version }

let equal { dune_version } t =
  Dune_lang.Syntax.Version.equal dune_version t.dune_version

let hash { dune_version } = Dune_lang.Syntax.Version.hash dune_version

let dune_version t = t.dune_version

let to_dyn { dune_version } =
  Dyn.Record [ ("dune_version", Dune_lang.Syntax.Version.to_dyn dune_version) ]

let should_remove_write_permissions_on_generated_files t =
  t.dune_version >= (2, 4)

let should_expand_aliases_when_sandboxing t = t.dune_version >= (3, 0)
