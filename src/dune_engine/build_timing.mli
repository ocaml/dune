open Import

val format : build_duration:Time.Span.t -> process_time:Time.Span.t -> string
val format_now : Time.t -> string
