Defining a Library with C Stubs
===============================

Assuming you have a file called ``mystubs.c``, that you need to pass
``-I/blah/include`` to compile it and ``-lblah`` at link time, write
this ``dune`` file:

.. code:: dune

    (library
     (name            mylib)
     (public_name     mylib)
     (libraries       re lwt)
     (foreign_stubs
      (language c)
      (names mystubs)
      (flags -I/blah/include))
     (c_library_flags (-lblah)))
