module Mode = struct
  type t =
    | Binary
    | Text
    | Text_jbuild

  let of_kind : Dune_lang.File_syntax.t -> t =
    function
    | Jbuild -> Text_jbuild
    | Dune   -> Text
end

type 'path t =
  { optional : bool
  ; mode     : Mode.t
  ; file1    : 'path
  ; file2    : 'path
  }

let decode path ~optional =
  let open Dune_lang.Decoder in
  let+ file1 = path
  and+ file2 = path
  and+ kind = Stanza.file_kind ()
  in
  let mode = Mode.of_kind kind in
  { optional ; file1; file2; mode }

let decode_binary path =
  let open Dune_lang.Decoder in
  let+ () = Syntax.since Stanza.syntax (1, 0)
  and+ file1 = path
  and+ file2 = path
  in
  { optional = false; file1; file2; mode = Binary }
