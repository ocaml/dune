open Import

let env = Fdecl.create Env.to_dyn

let init initenv =
  Fdecl.set env
    (initenv
    |> Env.add ~var:"INSIDE_DUNE" ~value:"1"
       (* To improve reproducibility, we don't let command executed by Dune
          observe whether Dune is run inside emacs or not. One such program that
          behave differently when run inside emacs is Dune itself and we sometimes
          run Dune from inside Dune, for instance in cram tests, so it is
          important to do this. *)
    |> Env.remove ~var:"INSIDE_EMACS")

let env () = Fdecl.get env
