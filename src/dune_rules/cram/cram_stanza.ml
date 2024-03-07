open Import
open Dune_lang.Decoder

type applies_to =
  | Whole_subtree
  | Files_matching_in_this_dir of Predicate_lang.Glob.t

let default_applies_to = Files_matching_in_this_dir Predicate_lang.true_

let decode_applies_to =
  let open Dune_lang.Decoder in
  let subtree =
    let+ _ = keyword ":whole_subtree" in
    Whole_subtree
  in
  let predicate =
    let+ predicate = Predicate_lang.Glob.decode in
    Files_matching_in_this_dir predicate
  in
  subtree <|> predicate
;;

type t =
  { loc : Loc.t
  ; applies_to : applies_to
  ; alias : Alias.Name.t option
  ; deps : Dep_conf.t Bindings.t option
  ; enabled_if : Blang.t
  ; locks : Locks.t
  ; package : Package.t option
  ; runtest_alias : (Loc.t * bool) option
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let decode =
  let* () = Dune_lang.Syntax.since Stanza.syntax (2, 7)
  and+ project = Dune_project.get_exn ()
  and+ loc = loc in
  let* () =
    let+ () = return () in
    if not (Dune_project.cram project)
    then
      User_warning.emit
        ~loc
        ~is_error:(Dune_project.dune_version project >= (3, 0))
        [ Pp.text "Cram tests are not enabled in this project." ]
        ~hints:
          [ Pp.text
              "You can enable cram tests by adding (cram enable) to your dune-project \
               file."
          ]
  in
  fields
    (let+ applies_to = field "applies_to" decode_applies_to ~default:default_applies_to
     and+ alias = field_o "alias" Dune_lang.Alias.decode
     and+ deps = field_o "deps" (Bindings.decode Dep_conf.decode)
     and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:None ()
     and+ locks = Locks.field ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 9)) ()
     and+ package =
       Stanza_common.Pkg.field_opt ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 8)) ()
     and+ runtest_alias =
       field_o
         "runtest_alias"
         (Dune_lang.Syntax.since Stanza.syntax (3, 12) >>> located bool)
     in
     { loc; alias; deps; enabled_if; locks; applies_to; package; runtest_alias })
;;

let stanza =
  [ ( "cram"
    , let+ t = decode in
      List.singleton (make_stanza t) )
  ]
;;
