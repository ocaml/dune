explicit_js_mode
----------------

.. describe:: (explicit_js_mode ...)

   Do not implicitly add ``js`` to the ``(modes ...)`` field of executables.

   In projects that use dune lang 1.x, JavaScript targets are defined for every
   bytecode executable. This is not very precise and does not interact well
   with the :doc:`/reference/aliases/all` alias.

   It is possible to opt out of this behavior by using:

   .. code:: dune

       (explicit_js_mode)

   When this is enabled, an explicit ``js`` mode needs to be added to the
   ``(modes ...)`` field of executables in order to trigger the JavaScript
   compilation. Explicit JS targets declared like this will be attached to the
   :doc:`/reference/aliases/all` alias.

   Starting with Dune 2.0, this behavior is the default, and there is no way to
   disable it.
