pin
===

.. warning::

   Dune Package Management is not final yet and the configuration options
   are subject to change.

This stanza is used to define additional package sources.

.. note::

   Defining a pin does not enable it by default. It needs to be enabled in a
   lock directory using the :doc:`/reference/dune-workspace/lock_dir` stanza.

.. describe:: (pin ...)

   .. versionadded:: 3.15

   Defines a new package source.

   .. describe:: (name <string>)

      The name of the newly defined pin. This can be anything, it does not
      have to match the package.

   .. describe:: (url <string>)

      The location of the sources for the pin.

   .. describe:: (package ...)

      Specifies the the packages to assign this pin to.

      .. describe:: (name <string>)

         The name of the package.

         This must be specified.

      .. describe:: (version <string>)

         The version that the package should be assumed to be.

.. seealso:: :doc:`pin stanza in dune-project </reference/dune-project/pin>` for
   per-project pins.
