open Import

module Module_system = struct
  type t =
    | ESM
    | CommonJS

  let default = CommonJS, Filename.Extension.js

  let to_string = function
    | ESM -> "es6"
    | CommonJS -> "commonjs"
  ;;
end

module Cm_kind = Dune_lang.Melange.Cm_kind

module Source = struct
  let dir = ".melange_src"
end

module Install = struct
  let dir = "melange"

  let maybe_prepend_melange_install_dir =
    let melange_install_dir = dir in
    fun ~for_ dir ->
      match for_ with
      | Compilation_mode.Ocaml -> dir
      | Melange ->
        let base = Path.Local.of_string melange_install_dir in
        (match dir with
         | None -> Some base
         | Some dir -> Some (Path.Local.append base dir))
  ;;
end
