open Import

(* flags are duplicated because we want to have two sets of default (:standard)
   flags. [link_flags_cxx] will be used for executable with foreign_cxx when
   [use_standard_cxx_flags] is true *)
type 'a t =
  { link_flags : 'a
  ; link_flags_cxx : 'a
  }

module Spec = struct
  type nonrec t = Ordered_set_lang.Unexpanded.t t

  let standard =
    let standard = Ordered_set_lang.Unexpanded.standard in
    { link_flags = standard; link_flags_cxx = standard }
  ;;

  let decode ~check =
    let open Decoder in
    let+ flags = Ordered_set_lang.Unexpanded.field "link_flags" ?check in
    { link_flags = flags; link_flags_cxx = flags }
  ;;

  let equal { link_flags; link_flags_cxx } t =
    Ordered_set_lang.Unexpanded.equal link_flags t.link_flags
    && Ordered_set_lang.Unexpanded.equal link_flags_cxx t.link_flags_cxx
  ;;
end

let get ~use_standard_cxx_flags (t : _ t) =
  if use_standard_cxx_flags then t.link_flags_cxx else t.link_flags
;;
