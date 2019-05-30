let placeholders = []

type value =
  | Unset
  | Direct of string
  | Placeholder of string ref

let version = Direct "bootstrap"
let statically_linked_libraries = []
