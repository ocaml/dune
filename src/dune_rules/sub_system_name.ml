open Import
module T = Lib_name
include T

include Stringlike.Make (struct
    include T

    let module_ = "Sub_system_name"
    let description = "the name of subsystems"
    let description_of_valid_string = None
    let hint_valid = None
    let of_string_opt = Lib_name.of_string_opt
  end)

include Comparable.Make (T)
