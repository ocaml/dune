@all
====

This alias corresponds to every known file target in a directory.

Since version 2.0 of the dune language, JS targets of executables are no longer
included in the `all` alias by default. To get back the old behavior of
including the JS targets in `all`, one can add the ``js`` target to the
executable's ``modes`` field.
