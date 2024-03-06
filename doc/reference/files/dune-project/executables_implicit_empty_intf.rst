executables_implicit_empty_intf
-------------------------------

.. describe:: (executables_implicit_empty_intf ...)

   .. versionadded:: 2.9

   Automatically generate empty interface files for executables and tests that
   do not already have them.

   By default, executables defined via ``(executables(s) ...)`` or ``(test(s)
   ...)`` stanzas are compiled with the interface file provided (e.g., ``.mli``
   or ``rei``). Since these modules cannot be used as library dependencies,
   it is common to give them empty interface files to strengthen the compiler's
   ability to detect unused values in these modules.

   This option, when enabled, will generate an empty `*.mli` file.

   Example:

   .. code:: dune

       (executables_implicit_empty_intf true)

   This option is enabled by default starting with Dune lang 3.0.
