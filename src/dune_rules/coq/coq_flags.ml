open Import

type t =
  { coq_flags : string list
  ; coqdoc_flags : string list
  }

let default = { coq_flags = [ "-q" ]; coqdoc_flags = [ "--toc" ] }

let dump { coq_flags; coqdoc_flags } =
  List.map
    ~f:Dune_lang.Encoder.(pair string (list string))
    [ "coq_flags", coq_flags; "coqdoc_flags", coqdoc_flags ]
;;
