open! Stdune

type t = ..

module Parser = struct
  type nonrec t = string * t list Dune_lang.Decoder.t
end

let latest_version = (2, 1)

let syntax =
  Dune_lang.Syntax.create ~name:"dune" ~desc:"the dune language"
    [ (1, 12); latest_version ]
