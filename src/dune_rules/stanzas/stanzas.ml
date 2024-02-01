open Import
open Dune_lang.Decoder

(* Deprecated *)
module Jbuild_version = struct
  type t = V1

  let decode = enum [ "1", V1 ]
end

let () =
  Dune_project.Extension.register_deleted ~name:"library_variants" ~deleted_in:(2, 6)
;;

module Include = struct
  type t = Loc.t * string

  let decode =
    let+ loc = loc
    and+ fn = relative_file in
    loc, fn
  ;;

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)
end

module Dynamic_include = struct
  type t = Include.t

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)
end

let rules l = List.map l ~f:(fun x -> Rule_conf.make_stanza x)
let execs exe = [ Executables.make_stanza exe ]

type constructors = Stanza.Parser.t list

let stanzas : constructors =
  [ Site_stanzas.all
  ; Cram_stanza.stanza
  ; List.map Dune_file0.statically_evaluated_stanzas ~f:(fun stanza ->
      ( stanza
      , let* loc = loc in
        User_error.raise
          ~loc
          [ Pp.textf
              "stanza %S may not appear in (include ..) or be generated with OCaml \
               syntax dune files"
              stanza
          ] ))
  ; [ ( "library"
      , let+ x = Library.decode in
        let base = [ Library.make_stanza x ] in
        match Library_redirect.Local.of_lib x with
        | None -> base
        | Some r -> Library_redirect.Local.make_stanza r :: base )
    ; ( "foreign_library"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 0)
        and+ x = Foreign.Library.decode in
        [ Foreign.Library.make_stanza x ] )
    ; "executable", Executables.single >>| execs
    ; "executables", Executables.multi >>| execs
    ; ( "rule"
      , let+ loc = loc
        and+ x = Rule_conf.decode in
        [ Rule_conf.make_stanza { x with loc } ] )
    ; ( "ocamllex"
      , let+ loc = loc
        and+ x = Rule_conf.ocamllex in
        rules (Rule_conf.ocamllex_to_rule loc x) )
    ; ( "ocamlyacc"
      , let+ loc = loc
        and+ x = Rule_conf.ocamlyacc in
        rules (Rule_conf.ocamlyacc_to_rule loc x) )
    ; ( "install"
      , let+ x = Install_conf.decode in
        [ Install_conf.make_stanza x ] )
    ; ( "alias"
      , let+ x = Alias_conf.decode in
        [ Alias_conf.make_stanza x ] )
    ; ( "copy_files"
      , let+ x = Copy_files.decode in
        [ Copy_files.make_stanza x ] )
    ; ( "copy_files#"
      , let+ x = Copy_files.decode in
        [ Copy_files.make_stanza { x with add_line_directive = true } ] )
    ; ( "include"
      , let+ loc = loc
        and+ fn = relative_file in
        [ Include.make_stanza (loc, fn) ] )
    ; ( "documentation"
      , let+ d = Documentation.decode in
        [ Documentation.make_stanza d ] )
    ; ( "jbuild_version"
      , let+ () = Dune_lang.Syntax.deleted_in Stanza.syntax (1, 0)
        and+ _ = Jbuild_version.decode in
        [] )
    ; ( "tests"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
        and+ t = Tests.multi in
        [ Tests.make_stanza t ] )
    ; ( "test"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
        and+ t = Tests.single in
        [ Tests.make_stanza t ] )
    ; ( "external_variant"
      , let+ () = Dune_lang.Syntax.deleted_in Stanza.syntax (2, 6) in
        [] )
    ; ( "env"
      , let+ x = Dune_env.decode in
        [ Dune_env.make_stanza x ] )
    ; ( "include_subdirs"
      , let* project = Dune_project.get_exn () in
        let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 1)
        and+ t =
          let enable_qualified = Dune_project.is_extension_set project Coq_stanza.key in
          Include_subdirs.decode ~enable_qualified
        and+ loc = loc in
        [ Include_subdirs.make_stanza (loc, t) ] )
    ; ( "toplevel"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 7)
        and+ t = Toplevel_stanza.decode in
        [ Toplevel_stanza.make_stanza t ] )
    ; ( "deprecated_library_name"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (2, 0)
        and+ t = Deprecated_library_name.decode in
        [ Deprecated_library_name.make_stanza t ] )
    ; ( "dynamic_include"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (3, 14)
        and+ include_ = Include.decode in
        [ Dynamic_include.make_stanza include_ ] )
    ]
  ]
  |> List.concat
;;

let () = Dune_project.Lang.register Stanza.syntax stanzas
let parse parser = Dune_lang.Decoder.parse parser Univ_map.empty

let of_ast (project : Dune_project.t) sexp =
  let parser = Dune_project.stanza_parser project in
  parse parser sexp
;;

let stanza_package stanza =
  match Stanza.repr stanza with
  | Library.T lib -> Library.package lib
  | Alias_conf.T { package = Some package; _ }
  | Rule_conf.T { package = Some package; _ }
  | Install_conf.T { package; _ }
  | Plugin.T { package; _ }
  | Executables.T { install_conf = Some { package; _ }; _ }
  | Documentation.T { package; _ }
  | Tests.T { package = Some package; _ } -> Some package
  | Coq_stanza.Theory.T { package = Some package; _ } -> Some package
  | _ -> None
;;
