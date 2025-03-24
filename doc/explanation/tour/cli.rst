Command-Line Interface
----------------------

The command-line interface is defined using `cmdliner
<https://erratique.ch/software/cmdliner>`_. One thing to note is that we use
binding operators to compose terms:

`bin/print_rules.ml <https://github.com/ocaml/dune/blob/3.15.0/bin/print_rules.ml#L174-L190>`_
  .. code-block:: ocaml
    :linenos:
    :lineno-start: 174

    let+ builder = Common.Builder.term
    and+ out =
      Arg.(
        value
        & opt (some string) None
        & info [ "o" ] ~docv:"FILE" ~doc:"Output to a file instead of stdout.")
    and+ recursive =
      Arg.(
        value
        & flag
        & info
            [ "r"; "recursive" ]
            ~doc:
              "Print all rules needed to build the transitive dependencies of the given \
               targets.")
    and+ syntax = Syntax.term
    and+ targets = Arg.(value & pos_all dep [] & Arg.info [] ~docv:"TARGET") in
