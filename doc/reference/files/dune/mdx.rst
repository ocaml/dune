#####
 mdx
#####

.. versionadded:: 2.4

MDX is a tool that helps you keep your markdown documentation up-to-date
by checking that its code examples are correct. When setting an MDX
stanza, the MDX checks are automatically attached to the ``runtest``
alias of the stanza's directory.

See `MDX's repository <https://github.com/realworldocaml/mdx>`__ for
more details.

You can define an MDX stanza to specify which files you want checked.

Note that this feature is still experimental and needs to be enabled in
your ``dune-project`` with the following ``using`` stanza:

.. code:: dune

   (using mdx 0.4)

.. note::

   Version ``0.2`` of the stanza requires mdx ``1.9.0``. Version ``0.4``
   of the stanza requires mdx ``2.3.0``.

The syntax is as follows:

.. code:: dune

   (mdx <optional-fields>)

Where ``<optional-fields>`` are:

-  ``(files <globs>)`` are the files that you want MDX to check,
   described as a list of globs (see the :ref:`Glob language
   specification <glob>` ). It defaults to ``*.md *.mld`` as of version
   ``0.4`` of the stanza and ``*.md`` before.

-  ``(deps <deps-conf list>)`` to specify the dependencies of your
   documentation code blocks. See :doc:`/concepts/dependency-spec` for
   more details.

-  ``(preludes <files>)`` are the prelude files you want to pass to MDX.
   See `MDX's documentation <https://github.com/realworldocaml/mdx>`__
   for more details on preludes.

-  ``(libraries <libraries>)`` are libraries that should be statically
   linked in the MDX test executable.

-  ``(enabled_if <blang expression>)`` is the same as the corresponding
   field of :doc:`library`.

-  ``(package <package>)`` specifies which package to attach this stanza
   to (similarly to when ``(package)`` is attached to a ``(rule)``
   stanza). When ``-p`` is passed, ``(mdx)`` stanzas with another
   package will be ignored. Note that this feature is completely
   separate from ``(packages)``, which specifies some dependencies.

-  ``(locks <lock-names>)`` specifies that the action of running the
   tests holds the specified locks. See :doc:`/concepts/locks` for more
   details.

****************************
 Upgrading from Version 0.1
****************************

-  The 0.2 version of the stanza requires at least MDX 1.9.0. If you
   encounter an error such as, ``ocaml-mdx: unknown command
   `dune-gen'``, then you should upgrade MDX.

-  The field ``(packages <packages>)`` is deprecated in version 0.2. You
   can use package items in the generic ``deps`` field instead: ``(deps
   (package <package>) ... (package <package>))``

-  Use the new ``libraries`` field to directly link libraries in the
   test executable and remove the need for ``#require`` directives in
   your documentation code blocks.
