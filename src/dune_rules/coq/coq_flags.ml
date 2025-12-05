open Import

type t =
  { coq_flags : string list
  ; coqdep_flags : string list
  ; coqdoc_flags : string list
  ; coqdoc_header : Path.t option
  ; coqdoc_footer : Path.t option
  }

let default =
  { coq_flags = [ "-q" ]
  ; coqdep_flags = []
  ; coqdoc_flags = [ "--toc" ]
  ; coqdoc_header = None
  ; coqdoc_footer = None
  }
;;

let dump ~dir { coq_flags; coqdep_flags; coqdoc_flags; coqdoc_header; coqdoc_footer } =
  List.map
    ~f:Dune_lang.Encoder.(pair string (list string))
    [ "coq_flags", coq_flags; "coqdep_flags", coqdep_flags; "coqdoc_flags", coqdoc_flags ]
  @ List.map
      ~f:Dune_lang.Encoder.(pair string (option (Dune_lang.Path.Local.encode ~dir)))
      [ "coqdoc_header", coqdoc_header; "coqdoc_footer", coqdoc_footer ]
;;
