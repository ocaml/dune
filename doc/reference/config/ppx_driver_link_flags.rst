ppx_driver_link_flags
---------------------

Specifies extra flags to pass when linking ppx drivers, the executables that
Dune builds to run the ppx rewriters of a ``(preprocess (pps ...))`` field. This
includes the drivers that Dune installs for ``ppx_rewriter`` libraries. Other
executables are not affected.

.. code:: dune

    (ppx_driver_link_flags (<flags>))

For example, on Linux, linking the drivers without PIE avoids relocating
their data every time they start, which makes preprocessing cheaper when many
drivers run in parallel:

.. code:: dune

    (ppx_driver_link_flags (-ccopt -no-pie))

Like other config fields, it can also be set in ``dune-workspace``, where it
replaces the value from the config file. ``dune build -p`` does not read the
config file but does read ``dune-workspace``, so a value committed there also
applies to release builds of the project, on every platform.

.. versionadded:: 3.26
