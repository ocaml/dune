Library Dependencies
====================

Library dependencies are specified using ``(libraries ...)`` fields in
``library`` and ``executables`` stanzas.

For libraries defined in the current scope, you can either use the real name or
the public name. For libraries that are part of the :term:`installed world`, or
for libraries that are part of the current workspace but in another scope, you
need to use the public name. For instance: ``(libraries base re)``.

When resolving libraries, ones that are part of the workspace are always
preferred to ones that are part of the :term:`installed world`.

Alternative Dependencies
------------------------

Sometimes, one doesn't want to depend on a specific library but rather
on whatever is already installed, e.g., to use a different
backend, depending on the target.

Dune allows this by using a ``(select ... from ...)`` form inside the list
of library dependencies.

Select forms are specified as follows:

.. code:: dune

    (select <target-filename> from
     (<literals> -> <filename>)
     (<literals> -> <filename>)
     ...)

``<literals>`` are lists of literals, where each literal is one of:

- ``<library-name>``, which will evaluate to true if ``<library-name>`` is
  available, either in the workspace or in the :term:`installed world`
- ``!<library-name>``, which will evaluate to true if ``<library-name>`` is not
  available in the workspace or in the :term:`installed world`

When evaluating a select form, Dune will create ``<target-filename>`` by
copying the file given by the first ``(<literals> -> <filename>)`` case where
all the literals evaluate to true. It is an error if none of the clauses are
selectable. You can add a fallback by adding a clause of the form ``(->
<file>)`` at the end of the list.

Re-Exported Dependencies
------------------------

A dependency ``foo`` may be marked as always *re-exported* using the
following syntax:

.. code:: dune

   (re_export foo)

For instance:

.. code:: dune

   (library
    (name bar)
    (libraries (re_export foo)))

This states that this library explicitly re-exports the interface of
``foo``. Concretely, when something depends on ``bar``, it will also
be able to see ``foo`` independently of whether :doc:`implicit
transitive dependencies<dune-project/implicit_transitive_deps>` are
allowed or not. When they are allowed, which is the default, all transitive
dependencies are visible, whether they are marked as re-exported or not.

Instantiating Parameterised Dependencies
----------------------------------------

This feature requires OxCaml, see :doc:`/reference/dune/library_parameter`.

A parameterised dependency ``foo`` can be instantiated with the arguments
``bar``, ``qux`` using the syntax:

.. code:: dune

   (foo bar qux)

For example:

.. code:: dune

   (library
    (name test)
    (libraries (foo bar qux)))

The library ``foo`` must have declared the set of parameters it expects, and
the arguments given to the instantiation must implement a subset of these
parameters. The ordering of the arguments does not matter, as the instantiation
relies on the implemented parameter to uniquely identify each argument.
For executables, the parameterised dependencies must be fully instantiated.

In the OCaml code, the instantiated library will be available under the module
name ``Foo``. To avoiding overlapping module names when instantiating the same
dependency multiple times, the syntax ``:as`` allows renaming the module. For
example:

.. code:: dune

   (library
    (name test)
    (libraries
     (foo a   b   :as foo_a_b)
     (foo bar qux :as foo_bar_qux)))

Then the instantiations will be available under the names ``Foo_a_b`` and
``Foo_bar_qux``.

Dependencies automatically inherit the parameters of their parent library.
For example, assuming the parameterised library ``foo`` requires two
parameters ``p`` and ``q``:

.. code:: dune

   (library
    (name test)
    (parameters p q)
    (libraries
     (foo :as foo_implicit)
     (foo an_implementation_of_q :as foo_q)
     (foo bar qux :as foo_bar_qux)
     other_foo))

Then ``foo_implicit`` is implicitly ``(foo p q)``,
while ``(foo an_implementation_of_q)`` will only inherit the parameter ``p``.

If ``other_foo``, which is not explicitly instantiated here, is also
parameterised by the parameters ``p`` (and) or ``q``, it will also inherit
its parent arguments. Dune will report an error if a dependency requires
parameters which have neither been given explicitly given via an instantiation
and are not listed in the parent library parameters.

For unwrapped libaries, the instantiation of parameterised libraries is not
currently generated. This is subject to change soon, but in the mean time,
you'll need to manually declare the instantiations: If you depend on the
instantiation ``(foo bar qux :as new_name)`` with ``bar`` an implementation of
the parameter ``param_bar`` and ``qux`` an implementation of ``param_qux``,
then you'll need to write the following:

.. code:: ocaml

   module New_name = Foo (Param_bar) (Bar) (Param_qux) (Qux) [@jane.non_erasable.instances]

.. note::

   While this reuses the OCaml functor application syntax, the attribute changes
   the meaning: The ``(Param) (Impl)`` must go together as a pair, but the
   ordering of the arguments otherwise does not matter.
