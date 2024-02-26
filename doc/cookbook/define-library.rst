Defining a Library
==================

``dune``
  .. code:: dune
  
      (library
       (name mylib)
       (public_name mylib)
       (libraries re lwt))

The library will be composed of all the modules in the same directory.
Outside of the library, module ``Foo`` will be accessible as
``Mylib.Foo``, unless you write an explicit ``mylib.ml`` file.

You can then use this library in any other directory by adding ``mylib``
to the ``(libraries ...)`` field.
