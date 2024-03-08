wrapped_executables
-------------------

.. describe:: (wrapped_executables <bool>)

   .. versionadded:: 1.11

   Control wrapping of modules in executables.

   Executables are made of compilation units whose names may collide with
   libraries' compilation units. To avoid this possibility, Dune prefixes
   these compilation unit names with ``Dune__exe__``. This is entirely
   transparent to users except when such executables are debugged. In which
   case, the mangled names will be visible in the debugger.

   - with ``(wrapped_executables false)``, the original names are used.
   - with ``(wrapped_executables true)``, the names are mangled.

   Starting in language version 2.0, the default value is ``true``.
