module V1 = struct
  let unset = "n/a"

  let () =
    let process_placeholder p =
      let s = !p in
      let len = String.length s in
      let s =
        (* Parse the replacement format described in
           ../artifact_substitution.ml *)
        if s.[0] = '=' then
          let colon_pos = String.index_from s 1 ':' in
          let vlen = int_of_string (String.sub s 1 (colon_pos - 1)) in
          (* This [min] is because the value might have been truncated
             if it was too large *)
          let vlen = min vlen (len - colon_pos - 1) in
          String.sub s (colon_pos + 1) vlen
        else
          unset
      in
      p := s
    in
    List.iter process_placeholder Dune_build_info_data.placeholders

  let eval : Dune_build_info_data.value -> string = function
    | Unset -> unset
    | Direct s -> s
    | Placeholder p -> !p

  let version = eval Dune_build_info_data.version

  module Statically_linked_library = struct
    type t = string * Dune_build_info_data.value
    let name = fst
    let version (_, v) = eval v
  end

  let statically_linked_libraries = Dune_build_info_data.statically_linked_libraries
end
