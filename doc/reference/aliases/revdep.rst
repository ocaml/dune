@revdep, @revdep-check, @revdep-runtest, @revdep-install
========================================================

``@revdep`` builds the default alias of every local target that transitively
depends on any library defined in the current directory. The reverse
dependencies can be other libraries, executables, or tests; Dune walks the
dependency graph and schedules the appropriate alias in each dependent
directory.

Variants map to other standard aliases in the dependents:

- ``@revdep-check`` builds the ``@check`` alias of every reverse dependency.
- ``@revdep-runtest`` builds the ``@runtest`` alias of every reverse
  dependency.
- ``@revdep-install`` builds the ``@install`` alias of every reverse
  dependency.

Usage examples
--------------

- ``dune build @src/revdep`` builds the default alias of all targets that
  depend (directly or transitively) on any library declared in ``src``.
- ``dune build @src/revdep-check`` builds the corresponding ``@check`` alias
  for those reverse dependencies.

Notes
-----

- These aliases only do work when explicitly requested; they do not add
  overhead to regular builds.
- If multiple libraries live in the same ``dune`` file, their reverse
  dependencies are combined, so ``@dir/revdep`` covers dependents of any
  library in ``dir``.
