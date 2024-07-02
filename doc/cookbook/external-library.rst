Using an External Library
=========================

This example is similar to :doc:`hello-world` but will use Lwt (a concurrency
library) for outputting a message.

``dune``
  .. code:: dune

    (executable
     (name hello_world)
     (libraries lwt.unix))

``hello_world.ml``
  .. code:: ocaml

    Lwt_main.run (Lwt_io.printf "Hello, world!\n")

Run it!
-------

.. code:: console

  $ dune exec ./hello_world.exe
  Hello, world!
