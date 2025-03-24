open Import

type t =
  { coq_flags : string list
  ; coqdep_flags : string list
  ; coqdoc_flags : string list
  }

let default = { coq_flags = [ "-q" ]; coqdep_flags = []; coqdoc_flags = [ "--toc" ] }

let dump { coq_flags; coqdep_flags; coqdoc_flags } =
  List.map
    ~f:Dune_lang.Encoder.(pair string (list string))
    [ "coq_flags", coq_flags; "coqdep_flags", coqdep_flags; "coqdoc_flags", coqdoc_flags ]
;;
