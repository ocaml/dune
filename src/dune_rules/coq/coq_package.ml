(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) Inria 2019-2024                         *)
(* (c) CNRS 2025                               *)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Li-yao Xia                      *)

open Import

(* It is what it is *)
let rocq_package_file = "rocq-package"

type t = {
  theories: Coq_lib_name.t list
}

let make ~theories = {
  theories
}

let decode () = let open Dune_lang.Decoder in return (assert false)

let parse _path _lexbuf =
  let sexp = assert false in
  let open Dune_lang.Decoder in
  parse (decode ()) Univ_map.empty sexp

let pp t =
  let open Dune_lang in
  let theories xs = List.map xs ~f:(fun t -> Encoder.string (Coq_lib_name.wrapper t)) in
  let fields = Encoder.record_fields [
    Encoder.field_i "theories" theories t.theories
  ] in
  Pp.concat_map ~sep:Pp.newline ~f:Dune_lang.pp fields

let write t = Format.asprintf "%a\n" Pp.to_fmt (pp t)
(* let parse *)
