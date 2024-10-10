type t = OpamTypes.filtered_formula

let of_dependencies deps = Package_dependency.list_to_opam_filtered_formula deps
let to_filtered_formula v = v
let of_filtered_formula v = v
