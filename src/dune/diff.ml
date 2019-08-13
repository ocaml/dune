open Stdune

module Mode = struct
  type t =
    | Binary
    | Text

  let compare_files = function
    | Binary ->
        Io.compare_files
    | Text ->
        Io.compare_text_files
end

type 'path t =
  { optional : bool
  ; mode : Mode.t
  ; file1 : 'path
  ; file2 : 'path
  }

let decode path ~optional =
  let open Dune_lang.Decoder in
  let+ file1 = path
  and+ file2 = path in
  { optional; file1; file2; mode = Text }

let decode_binary path =
  let open Dune_lang.Decoder in
  let+ () = Syntax.since Stanza.syntax (1, 0)
  and+ file1 = path
  and+ file2 = path in
  { optional = false; file1; file2; mode = Binary }

let eq_files { optional; mode; file1; file2 } =
  (optional && not (Path.exists file2))
  || Mode.compare_files mode file1 file2 = Eq
