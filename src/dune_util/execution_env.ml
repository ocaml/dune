open Stdune

let inside_emacs = Option.is_some (Env.get Env.initial "INSIDE_EMACS")
let inside_ci = Option.is_some (Env.get Env.initial "CI")

module Inside_dune = struct
  type t =
    | Yes
    | In_context of Stdune.Path.Build.t

  let var = "INSIDE_DUNE"

  let value = function
    | Yes -> "1"
    | In_context b -> Path.to_absolute_filename (Path.build b)
  ;;
end

let inside_dune = Option.is_some (Env.get Env.initial Inside_dune.var)
