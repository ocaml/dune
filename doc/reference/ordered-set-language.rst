######################
 Ordered Set Language
######################

A few fields take an ordered set as argument and can be specified using
a small DSL.

Dune interprets this DSL into an ordered set of strings using the
following rules:

-  ``:standard`` denotes the standard value of the field when it's
   absent
-  an atom not starting with a ``:`` is a singleton containing only this
   atom
-  a list of sets is the concatenation of its inner sets
-  ``(<sets1> \ <sets2>)`` is the set composed of elements of
   ``<sets1>`` that do not appear in ``<sets2>``

In addition, some fields support the inclusion of an external file using
the syntax ``(:include <filename>)``. For instance, this is useful when
you need to run a script to figure out some compilation flags.
``<filename>`` is expected to contain a single S-expression and cannot
contain ``(:include ...)`` forms.

Note that inside an ordered set, a list's first element cannot be an
atom except if it starts with ``-`` or ``:``. The reason for this is
that we're planning to add simple programmatic features in the future so
that one may write:

.. code::

   (flags (if (>= %{ocaml_version} 4.06) ...))

This restriction will allow you to add this feature without introducing
breaking changes. If you want to write a list where the first element
doesn't start with ``-``, you can simply quote it: ``("x" y z)``.

Most fields using the ordered set language also support
:doc:`../concepts/variables`. Variables are expanded after the set
language is interpreted.
