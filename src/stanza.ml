open! Stdune

type t = ..

module Parser = struct
  type nonrec t = string * t list Dune_lang.Decoder.t
end

let latest_version = (2, 0)

let syntax =
  Syntax.create ~name:"dune" ~desc:"the dune language"
    [ (0, 0) (* Jbuild syntax *)
    ; (1, 12)
    ; latest_version
    ]

module File_kind = struct
  type t = Dune_lang.File_syntax.t = Jbuild | Dune

  let of_syntax = function
    | (0, _) -> Jbuild
    | (_, _) -> Dune
end

let file_kind () =
  let open Dune_lang.Decoder in
  Syntax.get_exn syntax >>| File_kind.of_syntax
