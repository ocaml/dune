open Import

type t = Preprocess.Without_instrumentation.t Preprocess.Per_module.t

val decode
  : Preprocess.Without_instrumentation.t Preprocess.Per_module.t Dune_lang.Decoder.t

val default : (Loc.t * Lib_name.t) Preprocess.t Module_name.Per_item.t
val no_lint : t
