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

module Cli = struct
  type t =
    { package_name : string
    ; package_output : string
    ; module_name : string
    ; module_type : string
    ; stop_after_cmj : string
    }

  let of_project project =
    let version =
      Dune_project.find_extension_version project Dune_lang.Melange.syntax
      |> Option.value_exn
    in
    if version >= (1, 0)
    then
      { package_name = "--mel-package-name"
      ; package_output = "--mel-package-output"
      ; module_name = "--mel-module-name"
      ; module_type = "--mel-module-type"
      ; stop_after_cmj = "--mel-stop-after-cmj"
      }
    else
      { package_name = "--bs-package-name"
      ; package_output = "--bs-package-output"
      ; module_name = "--bs-module-name"
      ; module_type = "--bs-module-type"
      ; stop_after_cmj = "--bs-stop-after-cmj"
      }
  ;;

  let promotes_in_source project =
    match Dune_project.find_extension_version project Dune_lang.Melange.syntax with
    | Some version -> version >= (1, 0)
    | None -> false
  ;;
end

module Cm_kind = Dune_lang.Melange.Cm_kind

let output_path ~target_dir source =
  Path.Build.append_source target_dir (Path.Build.drop_build_context_exn source)
;;

module Emit = struct
  type t =
    { output_dir : Path.Build.t
    ; stanza_dir : Path.Build.t
    ; alias : Alias.Name.t
    }
end

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
