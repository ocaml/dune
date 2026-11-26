Boolean Language
================

The Boolean language allows the user to define simple Boolean expressions that
Dune can evaluate. Here's a semiformal specification of the language:

.. productionlist:: blang
   op : '=' | '<' | '>' | '<>' | '>=' | '<='
   version-op : 'ver=' | 'ver<' | 'ver>' | 'ver<>' | 'ver>=' | 'ver<='
   expr : (and <expr>+)
        : (or <expr>+)
        : (<op> <template> <template>)
        : (<version-op> <template> <template>)
        : (not <expr>)
        : <template>

After an expression is evaluated, it must be exactly the string ``true`` or
``false`` to be considered as a Boolean. Any other value will be treated as an
error.

Below is a simple example of a condition expressing that the build
has a Flambda compiler, with the help of variable expansion, and is
targeting OSX:

.. code:: dune

   (and %{ocaml-config:flambda} (= %{ocaml-config:system} macosx))

Version Comparisons
-------------------

.. versionadded:: 3.26

The operators ``ver=``, ``ver<>``, ``ver<``, ``ver<=``, ``ver>``, and ``ver>=``
compare versions using `opam's version ordering
<https://opam.ocaml.org/doc/Manual.html#Version-ordering>`_. For example:

.. code:: dune

   (enabled_if (ver>= %{ocaml_version} 5.9.0))

This condition also holds for OCaml 5.10.0. Numeric components are compared
numerically, leading zeros do not affect their value, and ``~`` introduces a
prerelease that sorts before the corresponding release. For example,
``(ver= 5.09 5.9)`` and ``(ver< 5.9~beta1 5.9)`` both evaluate to ``true``.

The ordinary comparison operators retain their existing behavior. If templates
expand to multiple values, version comparisons compare the lists element by
element using version ordering.
