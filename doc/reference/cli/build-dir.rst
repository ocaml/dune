``--build-dir`` - Custom Build Directory
========================================

By default Dune places all build artifacts in the ``_build`` directory relative
to the user's workspace. However, one can customize this directory by using the
``--build-dir`` flag or the ``DUNE_BUILD_DIR`` environment variable.

.. code:: console

   $ dune build --build-dir _build-foo

   # this is equivalent to:
   $ DUNE_BUILD_DIR=_build-foo dune build

   # Absolute paths are also allowed
   $ dune build --build-dir /tmp/build foo.exe
