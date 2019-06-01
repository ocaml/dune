open Stdune

type t =
  { path : Action.program
  ; run_dir : Path.Build.t
  }

let get sctx ~loc ~dir =
  { path = Super_context.resolve_program ~dir sctx ~loc "refmt"
             ~hint:"try: opam install reason"
  ; run_dir = Super_context.build_dir sctx
  }

let run ?stdout_to t = Command.run ?stdout_to ~dir:(Path.build t.run_dir) t.path

let format t ~input ~output =
  run t ~stdout_to:output [Command.Args.Dep input]

let to_ocaml_ast t ~input ~output =
  run t ~stdout_to:output
    [ Command.Args.A "--print"
    ; A "binary"
    ; Dep input
    ]
