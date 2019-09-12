module Client = struct
  type t = unit

  let result = Stdune.Result.Error "Dune_memory is disabled during bootstrap."

  let promote _ _ _ _ _ = result

  let search _ _ = Dune_memory.Search_result.Not_found

  let set_build_dir _ _ = ()

  let teardown _ = ()
end
