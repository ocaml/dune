open Import
open Action_builder.O

type t = string list Action_builder.t Dune_lang.Link_flags.t

let default ~default_cxx_link_flags =
  let link_flags_cxx =
    let+ flags = default_cxx_link_flags in
    List.concat_map flags ~f:(fun f -> [ "-cclib"; f ])
  in
  { Dune_lang.Link_flags.link_flags = Action_builder.return []; link_flags_cxx }
;;

let make ~(spec : Dune_lang.Link_flags.Spec.t) ~(default : _ Dune_lang.Link_flags.t) ~eval
  =
  let f name x standard =
    Action_builder.memoize ~cutoff:(List.equal String.equal) name (eval x ~standard)
  in
  { Dune_lang.Link_flags.link_flags = f "link flags" spec.link_flags default.link_flags
  ; link_flags_cxx = f "link flags cxx" spec.link_flags_cxx default.link_flags_cxx
  }
;;

let get ~use_standard_cxx_flags (t : t) =
  if use_standard_cxx_flags then t.link_flags_cxx else t.link_flags
;;

let dump (t : t) =
  let+ link_flags = t.link_flags in
  List.map ~f:Dune_lang.Encoder.(pair string (list string)) [ "link_flags", link_flags ]
;;
