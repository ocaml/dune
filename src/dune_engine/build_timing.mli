open Import

val format : build_duration:Time.Span.t -> action_duration:Time.Span.t -> string
val format_now : Time.t -> string
