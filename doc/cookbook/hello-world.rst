Hello, World!
=============

``dune``
  .. code:: dune
  
      (executable
       (name hello_world))

``hello_world.ml``
  .. code:: ocaml
  
      print_endline "Hello, world!"

Run it!
-------

.. code:: console

   $ dune exec ./hello_world.exe
   Hello, world!

Change it!
----------

Now, we can change ``hello_world.ml`` to the following:

.. code:: ocaml

   print_endline "Goodbye, world!"

Running ``dune exec`` again will rebuild the executable and execute it:

.. code:: console

   $ dune exec ./hello_world.exe
   Goodbye, world!
