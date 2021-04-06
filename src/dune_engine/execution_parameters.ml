open Stdune

module T = struct
  type t =
    { dune_version : Dune_lang.Syntax.Version.t
    ; swallow_stdout_on_success : bool
    }

  let equal { dune_version; swallow_stdout_on_success } t =
    Dune_lang.Syntax.Version.equal dune_version t.dune_version
    && Bool.equal swallow_stdout_on_success t.swallow_stdout_on_success

  let hash { dune_version; swallow_stdout_on_success } =
    Hashtbl.hash
      (Dune_lang.Syntax.Version.hash dune_version, swallow_stdout_on_success)

  let to_dyn { dune_version; swallow_stdout_on_success } =
    Dyn.Record
      [ ("dune_version", Dune_lang.Syntax.Version.to_dyn dune_version)
      ; ("swallow_stdout_on_success", Bool swallow_stdout_on_success)
      ]
end

include T

let builtin_default =
  { dune_version = Stanza.latest_version; swallow_stdout_on_success = false }

let set_dune_version x t = { t with dune_version = x }

let set_swallow_stdout_on_success x t = { t with swallow_stdout_on_success = x }

let dune_version t = t.dune_version

let should_remove_write_permissions_on_generated_files t =
  t.dune_version >= (2, 4)

let should_expand_aliases_when_sandboxing t = t.dune_version >= (3, 0)

let swallow_stdout_on_success t = t.swallow_stdout_on_success
