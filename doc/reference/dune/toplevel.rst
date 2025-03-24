toplevel
--------

The ``toplevel`` stanza allows one to define custom toplevels. Custom toplevels
automatically load a set of specified libraries and are runnable like normal
executables. Example:

.. code:: dune

   (toplevel
    (name tt)
    (libraries str))

This will create a toplevel with the ``str`` library loaded. We may build and
run this toplevel with:

.. code:: console

   $ dune exec ./tt.exe

``(preprocess (pps ...))`` is the same as the ``(preprocess (pps ...))`` field
of :doc:`library`. Currently, ``action`` and ``future_syntax`` are not
supported in the toplevel.
