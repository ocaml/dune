open Import

type t = OpamHash.t

include (
  Stringlike.Make (struct
    type t = OpamHash.t

    let to_string x = OpamHash.to_string x
    let module_ = "Dune_pkg.Checksum"
    let description = "Cryptographic hash of a package"
    let description_of_valid_string = None
    let hint_valid = None

    let of_string_opt s =
      (* verify through OpamHash *)
      OpamHash.of_string_opt s
    ;;
  end) :
    Stringlike with type t := t)

let to_opam_hash v = v
let of_opam_hash v = v
let of_dune_digest dune_digest = OpamHash.md5 (Dune_digest.to_string dune_digest)

let pp v =
  let s = to_string v in
  Pp.text s
;;

let equal = OpamHash.equal

include Comparable.Make (struct
    type nonrec t = t

    let compare x y = Ordering.of_int (OpamHash.compare x y)
    let to_dyn = to_dyn
  end)
