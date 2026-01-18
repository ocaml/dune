module Module_system = struct
  type t =
    | ESM
    | CommonJS

  let default = CommonJS, ".js"

  let to_string = function
    | ESM -> "es6"
    | CommonJS -> "commonjs"
  ;;
end

module Cm_kind = Dune_lang.Melange.Cm_kind

module Install = struct
  let dir = "melange"
end
