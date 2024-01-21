open Import
open Dune_lang.Decoder

(* This file defines Dune types as well as the S-expression syntax for the
   various supported versions of the specification. *)

(* Deprecated *)
module Jbuild_version = struct
  type t = V1

  let decode = enum [ "1", V1 ]
end

let () =
  Dune_project.Extension.register_deleted ~name:"library_variants" ~deleted_in:(2, 6)
;;

module Deprecated_library_name = struct
  module Old_name = struct
    type deprecation =
      | Not_deprecated
      | Deprecated of { deprecated_package : Package.Name.t }

    type t = Public_lib.t * deprecation

    let decode =
      let+ public = Public_lib.decode ~allow_deprecated_names:true in
      let deprecation =
        let deprecated_package = Lib_name.package_name (Public_lib.name public) in
        if let name = Package.name (Public_lib.package public) in
           Package.Name.equal deprecated_package name
        then Not_deprecated
        else Deprecated { deprecated_package }
      in
      public, deprecation
    ;;
  end

  type t = Old_name.t Library_redirect.t

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let old_public_name (t : t) = Public_lib.name (fst t.old_name)

  let decode =
    fields
      (let+ loc = loc
       and+ project = Dune_project.get_exn ()
       and+ old_name = field "old_public_name" Old_name.decode
       and+ new_public_name = field "new_public_name" (located Lib_name.decode) in
       let () =
         let loc, old_name = (fst old_name).name in
         if Lib_name.equal (snd new_public_name) old_name
         then
           User_error.raise
             ~loc
             [ Pp.text "old_public_name cannot be the same as the new_public_name" ]
       in
       { Library_redirect.loc; project; old_name; new_public_name })
  ;;
end

module Include = struct
  type t = Loc.t * string

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)
end

module Stanzas = struct
  type t = Stanza.t list

  let rules l = List.map l ~f:(fun x -> Rule_conf.make_stanza x)
  let execs exe = [ Executables.make_stanza exe ]

  type constructors = Stanza.Parser.t list

  let stanzas : constructors =
    [ Site_stanzas.all
    ; Cram_stanza.stanza
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
      ]
    ]
    |> List.concat
  ;;

  let () = Dune_project.Lang.register Stanza.syntax stanzas

  let parser project =
    let syntax_parser = Dune_project.stanza_parser project in
    Dune_project.set project syntax_parser
  ;;

  let parse parser = Dune_lang.Decoder.parse parser Univ_map.empty

  let of_ast (project : Dune_project.t) sexp =
    let parser = parser project in
    parse parser sexp
  ;;

  (* XXX this is needed for evaluating includes generated by dune files written
     in OCaml syntax.*)
  let rec parse_file_includes ~stanza_parser ~context sexps =
    List.concat_map sexps ~f:(parse stanza_parser)
    |> Memo.List.concat_map ~f:(fun stanza ->
      match Stanza.repr stanza with
      | Include.T (loc, fn) ->
        let open Memo.O in
        let* sexps, context = Include_stanza.load_sexps ~context (loc, fn) in
        parse_file_includes ~stanza_parser ~context sexps
      | _ -> Memo.return [ stanza ])
  ;;

  let parse ~file ~dir (project : Dune_project.t) sexps =
    let stanza_parser = parser project in
    let warnings = Warning_emit.Bag.create () in
    let stanza_parser = Warning_emit.Bag.set warnings stanza_parser in
    let open Memo.O in
    let* stanzas =
      let context =
        Include_stanza.in_file
        @@
        match file with
        | Some f -> f
        | None ->
          (* TODO this is wrong *)
          Path.Source.relative dir Source_tree.Dune_file.fname
      in
      parse_file_includes ~stanza_parser ~context sexps
    in
    let (_ : bool) =
      List.fold_left stanzas ~init:false ~f:(fun env stanza ->
        match Stanza.repr stanza with
        | Dune_env.T e ->
          if env
          then
            User_error.raise
              ~loc:e.loc
              [ Pp.text "The 'env' stanza cannot appear more than once" ]
          else true
        | _ -> env)
    in
    let+ () = Warning_emit.Bag.emit_all warnings in
    stanzas
  ;;
end

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

type t =
  { dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Stanzas.t
  }

let is_promoted_rule =
  let is_promoted_mode version = function
    | Rule.Mode.Promote { only = None; lifetime; _ } ->
      if version >= (3, 5)
      then (
        match lifetime with
        | Unlimited -> true
        | Until_clean -> false)
      else true
    | _ -> false
  in
  fun version rule ->
    match Stanza.repr rule with
    | Rule_conf.T { mode; _ } | Menhir_stanza.T { mode; _ } ->
      is_promoted_mode version mode
    | _ -> false
;;

let parse sexps ~dir ~file ~project =
  let open Memo.O in
  let+ stanzas = Stanzas.parse ~file ~dir project sexps in
  let stanzas =
    if !Clflags.ignore_promoted_rules
    then (
      let version = Dune_project.dune_version project in
      List.filter stanzas ~f:(fun s -> not (is_promoted_rule version s)))
    else stanzas
  in
  { dir; project; stanzas }
;;

module Make_fold (M : Monad.S) = struct
  open M.O

  let rec fold_stanzas l ~init ~f =
    match l with
    | [] -> M.return init
    | t :: l -> inner_fold t t.stanzas l ~init ~f

  and inner_fold t inner_list l ~init ~f =
    match inner_list with
    | [] -> fold_stanzas l ~init ~f
    | x :: inner_list ->
      let* init = f t x init in
      inner_fold t inner_list l ~init ~f
  ;;
end

module Memo_fold = Make_fold (Memo)
module Id_fold = Make_fold (Monad.Id)

let fold_stanzas t ~init ~f = Id_fold.fold_stanzas t ~init ~f

let equal t { dir; project; stanzas } =
  Path.Source.equal t.dir dir
  && Dune_project.equal t.project project
  && List.equal Stanza.equal t.stanzas stanzas
;;

let hash = Poly.hash
let to_dyn = Dyn.opaque
let of_ast = Stanzas.of_ast
