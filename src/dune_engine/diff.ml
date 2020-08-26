open Stdune

module Mode = struct
  type t =
    | Binary
    | Text

  let compare_files = function
    | Binary -> Io.compare_files
    | Text -> Io.compare_text_files
end

type ('path, 'target) t =
  { optional : bool
  ; mode : Mode.t
  ; file1 : 'path
  ; file2 : 'target
  }

let decode path target ~optional =
  let open Dune_lang.Decoder in
  let+ file1 = path
  and+ file2 = target in
  { optional; file1; file2; mode = Text }

let decode_binary path target =
  let open Dune_lang.Decoder in
  let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
  and+ file1 = path
  and+ file2 = target in
  { optional = false; file1; file2; mode = Binary }

let eq_files { optional; mode; file1; file2 } =
  let file2 = Path.build file2 in
  (optional && not (Path.exists file2))
  || Mode.compare_files mode file1 file2 = Eq
