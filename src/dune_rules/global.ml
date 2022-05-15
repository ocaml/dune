open Import

let env = Fdecl.create Env.to_dyn

let init ~capture_outputs =
  Fdecl.set env
    (let env =
       if
         (not capture_outputs)
         || not (Lazy.force Ansi_color.stderr_supports_color)
       then Env.initial
       else Colors.setup_env_for_colors Env.initial
     in
     let env = Env.add env ~var:"INSIDE_DUNE" ~value:"1" in
     (* To improve reproducibility, we don't let command executed by Dune
        observe whether Dune is run inside emacs or not. One such program that
        behave differently when run inside emacs is Dune itself and we sometimes
        run Dune from inside Dune, for instance in cram tests, so it is
        important to do this. *)
     Env.remove env ~var:"INSIDE_EMACS")

let env () = Fdecl.get env
