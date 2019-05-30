module V1 = struct
  module Version = struct
    type t = string

    let to_string x = x
  end

  include Build_info_data

  module Statically_linked_library = struct
    type t = string * string option
    let name = fst
    let version = snd
  end
end
