module Memory = struct
  type t = unit

  let result = Result.Error "Dune_memory is disabled during bootstrap."

  let promote _ _ _ _ _ = result

  let search _ _ = result
end
