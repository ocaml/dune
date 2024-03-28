Building a Hello World Program in Bytecode
============================================

This declares the hello_world executable implemented by ``hello_world.ml`` to
be build as native (``.exe``) or bytecode (``.bc``) version.

``dune``
  .. code:: dune
  
      (executable
       (name hello_world)
       (modes byte exe))

``hello_world.ml``
  .. code:: ocaml
  
      print_endline "Hello, world!"

Run it!
-------

.. code:: console

  $ dune exec ./hello_world.bc

The executable will be built as ``_build/default/hello_world.bc``. This
bytecode version allows the usage of ``ocamldebug``.
