module V1 = struct
  module Version = struct
    type t = string

    let to_string x = x
  end

  module Statically_linked_library = struct
    type t = string * string option

    let name = fst

    let version = snd
  end

  module Statically_linked_libraries = struct
    let to_list () = Build_info_data.statically_linked_libraries

    let find ~name =
      match List.assoc name (to_list ()) with
      | exception Not_found -> None
      | version -> Some (name, version)
  end

  let version () = Build_info_data.version

  module Path = struct
    type t = string

    let to_string t = t

    let of_string t = t

    let relative = Filename.concat
  end

  module Location = struct
    type t =
      | Etc
      | Share of { package : string }

    let component = function
      | Etc -> "etc"
      | Share { package } -> Printf.sprintf "share/%s" package
  end

  let registry_root =
    lazy
      ( match Sys.getenv_opt "DUNE_REGISTRY_ROOT" with
      | Some s -> s
      | None -> Build_info_data.artifact_root )

  let get l = Path.relative (Lazy.force registry_root) (Location.component l)
end
