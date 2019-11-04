open Stdune

module Client = struct
  type t = unit

  let result = Result.Error "Dune_cache is disabled during bootstrap."

  let promote _ _ _ _ _ = result

  let search _ _ = result

  let set_build_dir _ _ = ()

  let teardown _ = ()
end
