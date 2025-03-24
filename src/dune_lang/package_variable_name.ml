open Stdune
open Dune_sexp

module T = struct
  type t = OpamVariable.t

  let to_dyn s = Dyn.string (OpamVariable.to_string s)
  let compare x y = Ordering.of_int (OpamVariable.compare x y)
end

module Map = Map.Make (T)
module Set = Set.Make (T) (Map)
include T

let to_opam = Fun.id
let of_opam = Fun.id
let equal = OpamVariable.equal

include (
  Dune_util.Stringlike.Make (struct
    type t = OpamVariable.t

    let to_string x = OpamVariable.to_string x
    let module_ = "Variable_name"
    let description = "variable name"
    let description_of_valid_string = None
    let hint_valid = None
    let of_string_opt s = if s = "" then None else Some (OpamVariable.of_string s)
  end) :
    Dune_util.Stringlike with type t := t)

let encode t = Encoder.string (to_string t)

let decode =
  let open Decoder in
  string >>| of_string
;;

let arch = of_string "arch"
let os = of_string "os"
let os_version = of_string "os-version"
let os_distribution = of_string "os-distribution"
let os_family = of_string "os-family"
let opam_version = of_string "opam-version"
let sys_ocaml_version = of_string "sys-ocaml-version"
let with_test = of_string "with-test"
let with_doc = of_string "with-doc"
let with_dev_setup = of_string "with-dev-setup"
let version = of_string "version"
let name = of_string "name"
let build = of_string "build"
let post = of_string "post"
let one_of t xs = List.mem xs ~equal t
let dev = of_string "dev"

module Project = struct
  let encode name = Dune_sexp.Encoder.string (":" ^ to_string name)

  let decode =
    Dune_sexp.Decoder.atom_matching ~desc:"variable" (fun s ->
      if String.is_prefix s ~prefix:":" then Some (of_string (String.drop s 1)) else None)
  ;;
end
