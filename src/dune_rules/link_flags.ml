open Import
open Action_builder.O

(* flags are duplicated because we want to have two sets of default (:standard)
   flags. [link_flags_cxx] will be used for executable with foreign_cxx when
   [use_standard_cxx_flags] is true *)
type 'a t' =
  { link_flags : 'a
  ; link_flags_cxx : 'a
  }

module Spec = struct
  type t = Ordered_set_lang.Unexpanded.t t'

  let standard =
    let standard = Ordered_set_lang.Unexpanded.standard in
    { link_flags = standard; link_flags_cxx = standard }

  let decode ~since =
    let open Dune_lang.Decoder in
    let check =
      Option.map since ~f:(fun since ->
          Dune_lang.Syntax.since Stanza.syntax since)
    in
    let+ flags = Ordered_set_lang.Unexpanded.field "link_flags" ?check in
    { link_flags = flags; link_flags_cxx = flags }

  let equal { link_flags; link_flags_cxx } t =
    Ordered_set_lang.Unexpanded.equal link_flags t.link_flags
    && Ordered_set_lang.Unexpanded.equal link_flags_cxx t.link_flags_cxx
end

type t = string list Action_builder.t t'

let default ~default_cxx_link_flags =
  let link_flags_cxx =
    let+ flags = default_cxx_link_flags in
    List.concat_map flags ~f:(fun f -> [ "-cclib"; f ])
  in
  { link_flags = Action_builder.return []; link_flags_cxx }

let make ~spec ~default ~eval =
  let f name x standard =
    Action_builder.memoize ~cutoff:(List.equal String.equal) name
      (eval x ~standard)
  in
  { link_flags = f "link flags" spec.link_flags default.link_flags
  ; link_flags_cxx =
      f "link flags cxx" spec.link_flags_cxx default.link_flags_cxx
  }

let get ~use_standard_cxx_flags (t : t) =
  if use_standard_cxx_flags then t.link_flags_cxx else t.link_flags

let dump t =
  let+ link_flags = t.link_flags in
  List.map
    ~f:Dune_lang.Encoder.(pair string (list string))
    [ ("link_flags", link_flags) ]
