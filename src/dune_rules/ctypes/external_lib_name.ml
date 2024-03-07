open Import

include Stringlike.Make (struct
    type t = string

    let description = "external lib name"
    let module_ = "External_lib_name"
    let description_of_valid_string = None
    let hint_valid = None
    let to_string s = s
    let of_string_opt s = Some s
  end)

let equal = String.equal
let compare = String.compare

let clean t =
  String.init (String.length t) ~f:(fun i ->
    match t.[i] with
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'') as c -> c
    | _ -> '_')
;;

let to_module_name t = Module_name.of_string (clean t)
