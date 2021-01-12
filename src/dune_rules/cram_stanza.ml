open! Dune_engine
open! Stdune
open Import
open Dune_lang.Decoder

type applies_to =
  | Whole_subtree
  | Files_matching_in_this_dir of Predicate_lang.Glob.t

let default_applies_to = Files_matching_in_this_dir Predicate_lang.Glob.true_

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

type t =
  { loc : Loc.t
  ; applies_to : applies_to
  ; alias : Alias.Name.t option
  ; deps : Dep_conf.t Bindings.t option
  ; enabled_if : Blang.t
  ; package : (Loc.t * Package.t) option
  }

let decode =
  fields
    (let+ loc = loc
     and+ applies_to =
       field "applies_to" decode_applies_to ~default:default_applies_to
     and+ alias = field_o "alias" Alias.Name.decode
     and+ deps = field_o "deps" (Bindings.decode Dep_conf.decode)
     and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:None ()
     and+ project = Dune_project.get_exn ()
     and+ package_name =
       field_o "package"
         ( Dune_lang.Syntax.since Stanza.syntax (2, 8)
         >>> Package.Name.decode_loc )
     in
     let package =
       let open Option.O in
       let+ loc, name = package_name in
       let packages = Dune_project.packages project in
       match Package.Name.Map.find packages name with
       | Some pkg -> (loc, pkg)
       | None ->
         User_error.raise ~loc
           [ Pp.textf "The current scope doesn't define package %S"
               (Package.Name.to_string name)
           ]
     in
     { loc; alias; deps; enabled_if; applies_to; package })
