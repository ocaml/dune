open! Dune_engine
open Stdune

let env = Fdecl.create Env.to_dyn

let init ~capture_outputs =
  Fdecl.set env
    (let env =
       if
         (not capture_outputs)
         || not (Lazy.force Ansi_color.stderr_supports_color)
       then
         Env.initial
       else
         Colors.setup_env_for_colors Env.initial
     in
     Env.add env ~var:"INSIDE_DUNE" ~value:"1")

let env () = Fdecl.get env
