type t = Lib_dep.t list

val decode :
     allow_re_export:bool
  -> (Lib_dep.t list, Dune_lang.Decoder.values) Dune_lang.Decoder.parser

val of_pps : Lib_name.t list -> Lib_dep.t list
