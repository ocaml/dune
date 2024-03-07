(** Dune describe commands used to take a --lang argument that did nothing
    expect for dune describe workspace. To keep compatilbility with accepting
    such an argument we provide a dummy argument here that can be used. It's
    value will typically be ignored. *)
val arg : string option Cmdliner.Term.t
