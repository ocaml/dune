Using a PPX rewriter
====================

This is a sequel to :doc:`external-library` where in addition to an external
library, we will use a PPX rewriter.

``dune``
  .. code:: dune
  
      (executable
       (name hello_world)
       (libraries core)
       (preprocess (pps ppx_jane)))

``hello_world.ml``
  .. code:: ocaml
  
      open Core
  
      let () =
        Sexp.to_string_hum [%sexp ([3;4;5] : int list)]
        |> print_endline

Run it!
-------

.. code:: console

  $ dune exec ./hello_world.exe
  (3 4 5)
