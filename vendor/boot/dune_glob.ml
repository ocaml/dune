module V1 = struct
  type t = unit

  let empty = ()

  let universal = ()

  let unimplemented () = failwith "globs are not supported during bootstrap"

  let test _ _ = unimplemented ()

  let to_string _ = unimplemented ()

  let of_string _ = unimplemented ()

  let of_string_result _ = unimplemented ()
end
