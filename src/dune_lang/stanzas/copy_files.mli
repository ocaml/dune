open Import

type origin =
  | Build
  | Source

type t =
  { add_line_directive : bool
  ; alias : Alias_name.t option
  ; mode : Rule_mode.t
  ; enabled_if : Blang.t
  ; files : String_with_vars.t
  ; only_sources : Blang.t
  ; syntax_version : Syntax.Version.t
  }

include Stanza.S with type t := t

val decode : t Decoder.t
