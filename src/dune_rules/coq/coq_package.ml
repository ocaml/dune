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

type meta = {
  name: Loc.t * Coq_lib_name.t;
  coq_lang_version: Syntax.Version.t;
  package: Package.t option;
  boot: bool;
  use_stdlib: bool;
  plugins: (Loc.t * Lib_name.t) list;
  theories: (Loc.t * Coq_lib_name.t) list;
  loc: Loc.t;
}

type t = {
  path: Path.t;
  vo: Path.t list;
  meta: meta;
}

let of_stanza (s : Coq_stanza.Theory.t) =
  { name = s.name;
    coq_lang_version = s.buildable.coq_lang_version;
    package = s.package;
    boot = s.boot;
    use_stdlib = s.buildable.use_stdlib;
    plugins = s.buildable.plugins;
    theories = s.buildable.theories;
    loc = s.buildable.loc;
  }

let meta t = t.meta
let name t = snd t.meta.name
let path t = t.path
let vo t = t.vo

let decode () = let open Dune_lang.Decoder in return (assert false)

let parse _path _lexbuf =
  let sexp = assert false in
  let open Dune_lang.Decoder in
  parse (decode ()) Univ_map.empty sexp

let pp t =
  let open Dune_lang in
  let theories xs = List.map xs ~f:(fun t -> Encoder.string (Coq_lib_name.wrapper t)) in
  let fields = Encoder.record_fields [
    Encoder.field "name" Coq_lib_name.encode (snd t.name);
    Encoder.field_i "theories" theories (List.map ~f:snd t.theories);
    Encoder.field "use_stdlib" Encoder.bool t.use_stdlib;
  ] in
  Pp.concat_map ~sep:Pp.newline ~f:Dune_lang.pp fields

let write t = Format.asprintf "%a\n" Pp.to_fmt (pp t)
