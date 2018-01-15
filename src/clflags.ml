let concurrency = ref 4
(*let ocaml_comp_flags = ref ["-g"]*)
let g = ref true
let verbose = ref false
let debug_findlib = ref false
let warnings = ref "-40"
let debug_dep_path = ref false
let dev_mode = ref false
let workspace_root = ref "."
let external_lib_deps_hint = ref []
let capture_outputs = ref true
let debug_backtraces = ref false
let diff_command = ref None
module Promote_mode = struct
  type t =
    | Ignore
    | Check
    | Copy

  let to_string = function
    | Ignore -> "ignore"
    | Check -> "check"
    | Copy -> "copy"

  let of_string = function
    | "ignore" -> Some Ignore
    | "check" -> Some Check
    | "copy" -> Some Copy
    | _ -> None
end
let promote_mode = ref Promote_mode.Copy
