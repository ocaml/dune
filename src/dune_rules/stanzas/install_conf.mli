open Import

type t =
  { section : Loc.t * Section_with_site.t
  ; files : Install_entry.File.t list
  ; dirs : Install_entry.Dir.t list
  ; source_trees : Install_entry.Dir.t list
  ; package : Package.t
  ; enabled_if : Blang.t
  }

include Stanza.S with type t := t

val decode : t Dune_lang.Decoder.t
