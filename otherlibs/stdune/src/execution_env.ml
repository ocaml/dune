let inside_emacs = Option.is_some (Env.get Env.initial Env.Var._INSIDE_EMACS)
let inside_ci = Option.is_some (Env.get Env.initial (Env.Var.of_string "CI"))

module Inside_dune = struct
  type t =
    | Yes
    | In_context of Path.Build.t

  let var = Env.Var.of_string "INSIDE_DUNE"

  let value = function
    | Yes -> "1"
    | In_context b -> Path.to_absolute_filename (Path.build b)
  ;;
end

let inside_dune = Option.is_some (Env.get Env.initial Inside_dune.var)
