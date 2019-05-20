open Stdune
open Path

type t = Path.t

let encode p =
  let make constr arg =
    Dune_lang.List [
      Dune_lang.atom constr
    ; Dune_lang.atom_or_quoted_string arg
    ]
  in
  match p with
  | In_build_dir p ->
    make "In_build_dir" (Path.Build.to_string p)
  | In_source_tree p ->
    make "In_source_tree" (Path.Source.to_string p)
  | External p ->
    make "External" (Path.External.to_string p)

let decode =
  let open Dune_lang.Decoder in
  let external_ =
    plain_string (fun ~loc t ->
      if Filename.is_relative t then
        Dune_lang.Decoder.of_sexp_errorf loc "Absolute path expected"
      else
        Path.parse_string_exn ~loc t
    )
  in
  sum
    [ "In_build_dir"  , string    >>| Path.(relative build_dir)
    ; "In_source_tree", string    >>| Path.(relative root)
    ; "External"      , external_
    ]

module Local = struct
  let encode ~dir:from p =
    let open Dune_lang.Encoder in
    string (Path.reach ~from p)

  let decode ~dir =
    let open Dune_lang.Decoder in
    let+ (error_loc, path) = located string
    in
    Path.relative ~error_loc dir path
end
