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

module Conflict = struct
  type t =
    | Error
    | Ignore

  let to_string = function
    | Error -> "error"
    | Ignore -> "ignore"
  ;;

  let all = [ Error; Ignore ]
  let decode = enum (List.map all ~f:(fun x -> to_string x, x))
end

type t =
  { loc : Loc.t
  ; applies_to : applies_to
  ; alias : Alias.Name.t option
  ; deps : Dep_conf.t Bindings.t option
  ; enabled_if : Blang.t
  ; locks : Locks.t
  ; conflict : Conflict.t option
  ; package : Package.t option
  ; runtest_alias : (Loc.t * bool) option
  ; timeout : (Loc.t * float) option
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
       Stanza_pkg.field_opt ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 8)) ()
     and+ runtest_alias =
       field_o
         "runtest_alias"
         (Dune_lang.Syntax.since Stanza.syntax (3, 12) >>> located bool)
     and+ timeout =
       field_o
         "timeout"
         (Dune_lang.Syntax.since Stanza.syntax (3, 20)
          >>> located float
          >>| fun (loc, t) ->
          if t >= 0.
          then loc, t
          else
            User_error.raise
              ~loc
              [ Pp.text "Timeout value must be a non-negative float." ])
     and+ conflict =
       field_o
         "conflict"
         (Dune_lang.Syntax.since Stanza.syntax (3, 21) >>> Conflict.decode)
     in
     { loc
     ; alias
     ; deps
     ; enabled_if
     ; locks
     ; applies_to
     ; package
     ; runtest_alias
     ; timeout
     ; conflict
     })
;;

let stanza = [ "cram", decode_stanza decode ]
