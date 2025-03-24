open Import

module Old_name : sig
  type deprecation =
    | Not_deprecated
    | Deprecated of { deprecated_package : Package.Name.t }

  type t = Public_lib.t * deprecation
end

type t = Old_name.t Library_redirect.t

val decode : t Dune_lang.Decoder.t

include Stanza.S with type t := t

val old_public_name : t -> Lib_name.t
