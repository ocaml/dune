**********************
``dune-project`` files
**********************

These files are used to mark the root of projects as well as define project-wide
parameters. These files are required to have a ``lang`` which controls the names
and contents of all configuration files read by Dune. The ``lang`` stanza looks
like:

.. code:: scheme

          (lang dune 1.0)

Additionally, they can contains the following stanzas.

name
----

Sets the name of the project. This is used by :ref:`dune subst <dune-subst>`
and error messages.

.. code:: scheme

    (name <name>)

version
-------

Sets the version of the project:

.. code:: scheme

    (version <version>)

implicit_transitive_deps
------------------------

Enables or disables :ref:`implicit-transitive-deps`:

.. code:: scheme

    (implicit_transitive_deps <bool>)

wrapped_executables
-------------------

Enables or disables :ref:`wrapped-executables`:

.. code:: scheme

    (wrapped_executables <bool>)

explicit_js_mode
----------------

Enables :ref:`explicit-js-mode`:

.. code:: scheme

    (explicit_js_mode)
