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

type Stanza.t += T of t

let decode =
  fields
    (let+ loc = loc
     and+ applies_to = field "applies_to" decode_applies_to ~default:default_applies_to
     and+ alias = field_o "alias" Dune_lang.Alias.decode
     and+ deps = field_o "deps" (Bindings.decode Dep_conf.decode)
     and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:None ()
     and+ locks = Locks.field ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 9)) ()
     and+ package =
       Stanza_common.Pkg.field_opt ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 8)) ()
     and+ runtest_alias =
       field_o
         "runtest_alias"
         (Dune_lang.Syntax.since Stanza.syntax (3, 11) >>> located bool)
     in
     { loc; alias; deps; enabled_if; locks; applies_to; package; runtest_alias })
;;
