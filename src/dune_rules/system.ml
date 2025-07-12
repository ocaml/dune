open Import
open Fiber.O

module Spec = struct
  type (_, _) t = string

  let name = "system"
  let version = 1
  let bimap t _ _ = t
  let is_useful_to ~memoize = memoize
  let encode cmd _ _ : Sexp.t = Atom cmd

  let action cmd ~(ectx : Action.context) ~(eenv : Action.env) =
    let prog, arg =
      Env_path.system_shell_exn ~needed_to:"interpret (system ...) actions"
    in
    let display = !Clflags.display in
    Process.run
      (Accept eenv.exit_codes)
      prog
      [ arg; cmd ]
      ~display
      ~metadata:ectx.metadata
      ~stdout_to:eenv.stdout_to
      ~stderr_to:eenv.stderr_to
      ~stdin_from:eenv.stdin_from
      ~dir:eenv.working_dir
      ~env:eenv.env
    >>| function
    | Error _ -> ()
    | Ok s -> s
  ;;
end

module A = Action_ext.Make (Spec)

let action = A.action
