module File = struct
  type t =
    { in_the_memory : Stdune.Path.t
    ; in_the_build_directory : Stdune.Path.t
    ; digest : Digest.t
    }
end

module Search_result = struct
  type t =
    | Found of unit * File.t list
    | Not_found
    | Cannot_read of exn
end

module Memory = struct
  type t = unit

  let result = Stdune.Result.Error "Dune_memory is disabled during bootstrap."

  let promote _ _ _ _ _ = result

  let search _ _ = result
end
