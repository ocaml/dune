open! Stdune

type t = ..

module Parser = struct
  type nonrec t = string * t list Dune_lang.Decoder.t
end

let latest_version = (2, 6)

let since v = (v, `Since v)

let all_minors (major, minor) =
  List.init (minor + 1) ~f:(fun i -> since (major, i))

let syntax =
  Dune_lang.Syntax.create ~name:"dune" ~desc:"the dune language"
    (all_minors (1, 12) @ all_minors latest_version)
