open Stdune

module T = struct
  type t =
    { dune_version : Dune_lang.Syntax.Version.t
    ; swallow_stdout_on_success : bool
    ; fail_on_non_empty_stderr : bool
    }

  let equal
      { dune_version; swallow_stdout_on_success; fail_on_non_empty_stderr } t =
    Dune_lang.Syntax.Version.equal dune_version t.dune_version
    && Bool.equal swallow_stdout_on_success t.swallow_stdout_on_success
    && Bool.equal fail_on_non_empty_stderr t.fail_on_non_empty_stderr

  let hash { dune_version; swallow_stdout_on_success; fail_on_non_empty_stderr }
      =
    Hashtbl.hash
      ( Dune_lang.Syntax.Version.hash dune_version
      , swallow_stdout_on_success
      , fail_on_non_empty_stderr )

  let to_dyn
      { dune_version; swallow_stdout_on_success; fail_on_non_empty_stderr } =
    Dyn.Record
      [ ("dune_version", Dune_lang.Syntax.Version.to_dyn dune_version)
      ; ("swallow_stdout_on_success", Bool swallow_stdout_on_success)
      ; ("fail_on_non_empty_stderr", Bool fail_on_non_empty_stderr)
      ]
end

include T

let builtin_default =
  { dune_version = Stanza.latest_version
  ; swallow_stdout_on_success = false
  ; fail_on_non_empty_stderr = false
  }

let set_dune_version x t = { t with dune_version = x }

let set_swallow_stdout_on_success x t = { t with swallow_stdout_on_success = x }

let set_fail_on_non_empty_stderr x t = { t with fail_on_non_empty_stderr = x }

let dune_version t = t.dune_version

let should_remove_write_permissions_on_generated_files t =
  t.dune_version >= (2, 4)

let should_expand_aliases_when_sandboxing t = t.dune_version >= (3, 0)

let swallow_stdout_on_success t = t.swallow_stdout_on_success

let fail_on_non_empty_stderr t = t.fail_on_non_empty_stderr

let default = Fdecl.create Dyn.Encoder.opaque

let init t = Fdecl.set default t

let default =
  let open Memo.Build.O in
  let* () = Memo.Build.return () in
  Fdecl.get default
