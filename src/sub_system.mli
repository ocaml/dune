open! Import

include module type of struct include Sub_system_intf end

val register_multi_backends : (module Multi_backends) -> unit

val gen_rules : Library_compilation_context.t -> unit
