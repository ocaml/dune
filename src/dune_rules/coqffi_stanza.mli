open Import

type t =
  { modules : Module_name.t list
  ; loc : Loc.t
  ; library : Loc.t * Lib_name.t
  ; flags : Ordered_set_lang.t
  }

type Stanza.t += T of t
