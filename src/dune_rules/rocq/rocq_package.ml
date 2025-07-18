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

type meta =
  { name : Loc.t * Rocq_lib_name.t
  ; rocq_lang_version : Syntax.Version.t
  ; boot : bool
  ; use_stdlib : bool
  ; plugins : (Loc.t * Lib_name.t) list
  ; theories : (Loc.t * Rocq_lib_name.t) list
  ; loc : Loc.t
  }

type t =
  { path : Path.t
  ; vo : Path.t list
  ; meta : meta
  }

let of_stanza (s : Rocq_stanza.Theory.t) =
  { name = s.name
  ; rocq_lang_version = s.buildable.rocq_lang_version
  ; boot = s.boot
  ; use_stdlib = s.buildable.use_stdlib
  ; plugins = s.buildable.plugins
  ; theories = s.buildable.theories
  ; loc = s.buildable.loc
  }
;;

let meta t = t.meta
let name t = snd t.meta.name
let path t = t.path
let vo t = t.vo
let make ~vo ~path meta = { vo; path; meta }

let decode ~name:_ =
  let open Dune_sexp.Decoder in
  enter
    (fields
       (let+ loc = loc
        and+ rocq_lang_version = field "rocq" Syntax.Version.decode
        and+ name = field "name" Rocq_lib_name.decode
        and+ plugins = field "plugins" ~default:[] (repeat Lib_name.decode_loc)
        and+ theories = field "theories" ~default:[] (repeat Rocq_lib_name.decode)
        and+ use_stdlib = field "use_stdlib" bool
        and+ boot = field "boot" bool in
        { rocq_lang_version; name; theories; use_stdlib; boot; plugins; loc }))
;;

let of_file ~name path =
  let open Dune_sexp in
  let fields = Parser.load path ~mode:Parser.Mode.Many in
  Decoder.parse (decode ~name) Univ_map.empty (List (Loc.in_file path, fields))
;;

let pp t =
  let open Dune_lang in
  let theories = List.map ~f:(fun (_, name) -> Rocq_lib_name.encode name) in
  let plugins = List.map ~f:(fun (_, name) -> Lib_name.encode name) in
  let fields =
    Encoder.record_fields
      [ Encoder.field "rocq" Syntax.Version.encode t.rocq_lang_version
      ; Encoder.field "name" Rocq_lib_name.encode (snd t.name)
      ; Encoder.field_i "plugins" plugins t.plugins
      ; Encoder.field_i "theories" theories t.theories
      ; Encoder.field "use_stdlib" Encoder.bool t.use_stdlib
      ; Encoder.field "boot" Encoder.bool t.boot
      ]
  in
  Pp.concat_map ~sep:Pp.newline ~f:Dune_lang.pp fields
;;

let write t = Format.asprintf "%a\n" Pp.to_fmt (pp t)
