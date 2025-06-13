open Import

type t =
  { loc : Loc.t
  ; package : Package.t
  ; mld_files : Ordered_set_lang.t
  ; files : Install_entry.File.t list
  }

include Stanza.S with type t := t

val decode : t Dune_lang.Decoder.t
