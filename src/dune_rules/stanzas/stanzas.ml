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

let with_redirect decode =
  let+ (x : Library.t) = decode in
  let base =
    let package =
      (* CR rgrinberg: we need to check this *)
      match x.visibility with
      | Public p -> Some (Package.id p.package)
      | Private p -> Option.map p ~f:Package.id
    in
    [ Library.make_stanza x package ]
  in
  match Library_redirect.Local.of_lib x with
  | None -> base
  | Some r -> Library_redirect.Local.make_stanza r None :: base
;;

let stanzas : Stanza.Parser.t list =
  [ List.map Source.Dune_file.statically_evaluated_stanzas ~f:(fun stanza ->
      ( stanza
      , let* loc = loc in
        User_error.raise
          ~loc
          [ Pp.textf
              "stanza %S may not appear in (include ..) or be generated with OCaml \
               syntax dune files"
              stanza
          ] ))
  ; [ "library", with_redirect Library.decode
    ; "library_parameter", with_redirect Parameter.decode
    ; "foreign_library", Foreign_library.decode_stanza Foreign_library.decode
    ; ("cram", Cram_stanza.(decode_stanza decode))
    ; ("executable", Executables.(decode_stanza single))
    ; ("executables", Executables.(decode_stanza multi))
    ; ("rule", Rule_conf.(decode_stanza Rule_conf.decode))
    ; "ocamllex", Ocamllex.decode_stanza Ocamllex.decode
    ; "ocamlyacc", Ocamlyacc.decode_stanza Ocamlyacc.decode
    ; ("install", Install_conf.(decode_stanza decode))
    ; ("alias", Alias_conf.(decode_stanza decode))
    ; ("copy_files", Copy_files.(decode_stanza decode))
    ; ( "copy_files#"
      , Copy_files.decode_stanza
          (let+ x = Copy_files.decode in
           { x with add_line_directive = true }) )
    ; ("include", Include.(decode_stanza decode))
    ; ("documentation", Documentation.(decode_stanza Documentation.decode))
    ; ( "jbuild_version"
      , let+ () = Dune_lang.Syntax.deleted_in Stanza.syntax (1, 0)
        and+ _ = Jbuild_version.decode in
        [] )
    ; "tests", Tests.decode_stanza Tests.multi
    ; "test", Tests.decode_stanza Tests.single
    ; ( "external_variant"
      , let+ () = Dune_lang.Syntax.deleted_in Stanza.syntax (2, 6) in
        [] )
    ; ("env", Dune_env.(decode_stanza decode))
    ; ( "include_subdirs"
      , Include_subdirs.(
          decode_stanza
          @@ decode
               ~qualified:
                 (let* project = Dune_project.get_exn () in
                  if
                    Dune_project.is_extension_set project Coq_stanza.key
                    || Dune_project.is_extension_set project Rocq_stanza.key
                  then return ()
                  else Syntax.since Stanza.syntax (3, 7))) )
    ; "toplevel", Toplevel_stanza.decode_stanza Toplevel_stanza.decode
    ; ( "deprecated_library_name"
      , Deprecated_library_name.decode_stanza Deprecated_library_name.decode )
    ; ( "dynamic_include"
      , Dynamic_include.decode_stanza
          (let* () = Dune_lang.Syntax.since Stanza.syntax (3, 14) in
           Include.decode) )
    ; ("generate_sites_module", Generate_sites_module_stanza.(decode_stanza decode))
    ; ("plugin", Plugin.(decode_stanza Plugin.decode))
    ]
  ]
  |> List.concat
;;

let () = Dune_project.Lang.register Stanza.syntax stanzas

let stanza_package stanza =
  match
    (* Not clear if this check is even necessary. Shouldn't this always match
       [Stanza.package] anyway? *)
    (match Stanza.repr stanza with
     | Library.T lib -> Library.package lib
     | Alias_conf.T { package = Some package; _ }
     | Rule_conf.T { package = Some package; _ }
     | Install_conf.T { package; _ }
     | Plugin.T { package; _ }
     | Executables.T { install_conf = Some { package; _ }; _ }
     | Documentation.T { package; _ }
     | Tests.T { package = Some package; _ } -> Some package
     | Coq_stanza.Theory.T { package = Some package; _ } -> Some package
     | Rocq_stanza.Theory.T { package = Some package; _ } -> Some package
     | _ -> None)
    |> Option.map ~f:Package.id
  with
  | Some _ as s -> s
  | None -> Stanza.package stanza
;;
