***************
Instrumentation
***************

In this section, we'll explain how to define and use instrumentation backends
(such as ``bisect_ppx`` or ``landmarks``) so that you can enable and disable
coverage via ``dune-workspace`` files or by passing a command-line flag or
environment variable. In addition to providing an easy way to toggle
instrumentation of your code, this setup avoids creating a hard dependency on
the precise instrumentation backend in your project.

Specifying What to Instrument
=============================

When an instrumentation backend is activated, Dune will only instrument
libraries and executables for which the user has requested instrumentation.

To request instrumentation, one must add the following field to a library or
executable stanza:

.. code:: scheme

   (library
    (name ...)
    (instrumentation
     (backend <name> <args>)
     <optional-fields>))

The backend ``<name>`` can be passed into arguments using ``<args>``.

This field can be repeated multiple times in order to support various
backends. For instance:

.. code:: scheme

   (library
    (name foo)
    (modules foo)
    (instrumentation (backend bisect_ppx --bisect-silent yes))
    (instrumentation (backend landmarks)))

This will instruct Dune that when either the ``bisect_ppx`` or ``landmarks``
instrumentation is activated, the library should be instrumented with this
backend.

By default, these fields are simply ignored; however, when the corresponding
instrumentation backend is activated, Dune will implicitly add the relevant
``ppx`` rewriter to the list of ``ppx`` rewriters.

At the moment, it isn't possible to instrument code that's preprocessed via an
action preprocessors. As these preprocessors are quite rare nowadays, there is
no plan to add support for them in the future.

``<optional-fields>`` are:

- ``(deps <deps-conf list>)`` specifies extra instrumentation dependencies, for
  instance, if it reads a generated file. The dependencies are only applied
  when the instrumentation is actually enabled. The specification of
  dependencies is described in the :ref:`deps-field` section.

Enabling/Disabling Instrumentation
==================================

Activating an instrumentation backend can be done via the command line or the
``dune-workspace`` file.

Via the command line, it is done as follows:

.. code:: bash

   $ dune build --instrument-with <names>

Here ``<names>`` is a comma-separated list of instrumentation backends. For
example:

.. code:: bash

   $ dune build --instrument-with bisect_ppx,landmarks

This will instruct Dune to activate the given backend globally, i.e., in all
defined build contexts.

It's also possible to enable instrumentation backends via the
``dune-workspace`` file, either globally or for specific builds contexts.

To enable an instrumentation backend globally, type the following in your
``dune-workspace`` file:

.. code:: scheme

   (lang dune 3.7)
   (instrument_with bisect_ppx)

or for each context individually:

.. code:: scheme

   (lang dune 3.7)
   (context default)
   (context (default (name coverage) (instrument_with bisect_ppx)))
   (context (default (name profiling) (instrument_with landmarks)))

If both the global and local fields are present, the precedence is the same as
the ``profile`` field: the per-context setting takes precedence over the
command-line flag, which takes precedence over the global field.

Declaring an Instrumentation Backend
====================================

Instrumentation backends are libraries with the special field
``(instrumentation.backend)``. This field instructs Dune that the library can
be used as an instrumentation backend, and it also provides the parameters
specific to this backend.

Currently, Dune will only support ``ppx`` instrumentation tools, and the
instrumentation library must specify the ``ppx`` rewriters that instruments the
code. This can be done as follows:

.. code:: scheme

   (library
    ...
    (instrumentation.backend
      (ppx <ppx-rewriter-name>)))

When such an instrumentation backend is activated, Dune will implicitly add the
mentioned ``ppx`` rewriter to the list of ``ppx`` rewriters for libraries and
executables that specify this instrumentation backend.

.. _bisect_ppx: https://github.com/aantron/bisect_ppx
.. _landmarks: https://github.com/LexiFi/landmarks
