open Stdune

module Action_output_on_success = struct
  type t =
    | Print
    | Swallow
    | Must_be_empty

  let all =
    [ ("print", Print); ("swallow", Swallow); ("must-be-empty", Must_be_empty) ]

  let to_dyn = function
    | Print -> Dyn.Variant ("Print", [])
    | Swallow -> Variant ("Swallow", [])
    | Must_be_empty -> Variant ("Must_be_empty", [])

  let equal = Poly.equal

  let hash = Poly.hash
end

type t =
  { dune_version : Dune_lang.Syntax.Version.t
  ; action_stdout_on_success : Action_output_on_success.t
  ; action_stderr_on_success : Action_output_on_success.t
  }

let equal { dune_version; action_stdout_on_success; action_stderr_on_success } t
    =
  Dune_lang.Syntax.Version.equal dune_version t.dune_version
  && Action_output_on_success.equal action_stdout_on_success
       t.action_stdout_on_success
  && Action_output_on_success.equal action_stderr_on_success
       t.action_stderr_on_success

let hash { dune_version; action_stdout_on_success; action_stderr_on_success } =
  Hashtbl.hash
    ( Dune_lang.Syntax.Version.hash dune_version
    , Action_output_on_success.hash action_stdout_on_success
    , Action_output_on_success.hash action_stderr_on_success )

let to_dyn { dune_version; action_stdout_on_success; action_stderr_on_success }
    =
  Dyn.Record
    [ ("dune_version", Dune_lang.Syntax.Version.to_dyn dune_version)
    ; ( "action_stdout_on_success"
      , Action_output_on_success.to_dyn action_stdout_on_success )
    ; ( "action_stderr_on_success"
      , Action_output_on_success.to_dyn action_stderr_on_success )
    ]

let builtin_default =
  { dune_version = Stanza.latest_version
  ; action_stdout_on_success = Print
  ; action_stderr_on_success = Print
  }

let set_dune_version x t = { t with dune_version = x }

let set_action_stdout_on_success x t = { t with action_stdout_on_success = x }

let set_action_stderr_on_success x t = { t with action_stderr_on_success = x }

let dune_version t = t.dune_version

let should_remove_write_permissions_on_generated_files t =
  t.dune_version >= (2, 4)

let should_expand_aliases_when_sandboxing t = t.dune_version >= (3, 0)

let action_stdout_on_success t = t.action_stdout_on_success

let action_stderr_on_success t = t.action_stderr_on_success

let default = Fdecl.create Dyn.Encoder.opaque

let init t = Fdecl.set default t

let default =
  let open Memo.Build.O in
  let* () = Memo.Build.return () in
  Fdecl.get default
