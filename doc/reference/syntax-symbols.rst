Syntax Symbols
==============

Dune uses punctuation in several command-line and ``dune`` file syntaxes. This
page gives those symbols text names so they are easier to find with search.

If the documentation search box does not find a symbol directly, try searching
for its text name, such as "double at", "percent brace", or "colon standard".

.. _symbol-at:

Single at: ``@``
----------------

The single at sign names a recursive alias target on the command line. For
example, ``dune build @runtest`` builds the ``runtest`` alias in the current
directory and its subdirectories. See :doc:`aliases`.

.. _symbol-double-at:

Double at: ``@@``
-----------------

The double at sign names a non-recursive alias target on the command line. For
example, ``dune build @@runtest`` builds the ``runtest`` alias in the current
directory only. See :doc:`aliases`.

.. _symbol-percent-brace:

Percent brace: ``%{...}``
-------------------------

Percent-brace forms are variables. For example, ``%{target}`` expands to a rule
target and ``%{env:VAR=default}`` reads an environment variable. See
:doc:`../concepts/variables`.

.. _symbol-colon-standard:

Colon standard: ``:standard``
-----------------------------

``:standard`` denotes the default value in the :doc:`ordered set language
<ordered-set-language>` and in some :doc:`predicates <predicate-language>`.

.. _symbol-backslash:

Backslash: ``\``
----------------

In the ordered set language, ``\`` subtracts one set from another. For example,
``(:standard \ -w)`` removes ``-w`` from the standard set. See
:doc:`ordered-set-language`.

.. _symbol-include:

Colon include: ``(:include ...)``
---------------------------------

Some ordered-set fields use ``(:include <filename>)`` to read an ordered set
from a file. See :doc:`ordered-set-language`.

.. _symbol-arrow:

Arrow: ``->``
-------------

The arrow appears in ``select`` forms in library dependencies, where it
separates a list of library availability conditions from the file selected when
those conditions match. See :doc:`library-dependencies`.

.. _symbol-bang:

Bang: ``!``
-----------

In ``select`` forms, ``!<library-name>`` means that the library must not be
available. In glob patterns, ``[!<set>]`` matches a character not in ``<set>``.
See :doc:`library-dependencies` and :ref:`glob`.

.. _symbol-star:

Star: ``*`` and double star: ``**``
------------------------------------

Glob patterns use ``*`` and ``**`` to match file names. See :ref:`glob`.

.. _symbol-question:

Question mark: ``?``
--------------------

Glob patterns use ``?`` to match a single character. See :ref:`glob`.
