formatting
----------

.. describe:: (formatting ...)

   .. versionadded:: 2.0

   Control automatic formatting. Several forms are accepted:

   - To disable automatic formatting completely (equivalent to the behavior in
     language 1.x):

     .. code:: dune

        (formatting disabled)

   - To restrict the languages that are considered for formatting:

     .. code:: dune

        (formatting
         (enabled_for <languages>))

     The list of `<languages>` can be either ``dune`` (formatting of ``dune``
     files) or a :term:`dialect` name.

   .. seealso:: :doc:`/howto/formatting`

.. _format-generated-corrections:

.. describe:: (format_generated_corrections <bool>)

   .. versionadded:: 3.26

   Set this field to ``true`` in ``dune-project`` to format generated
   corrections before comparison or promotion. The default is ``false``.

   This applies to Cinaps, optional text diffs (``diff?``), inline tests, and
   PPX preprocessing and linting. Dune uses OCamlFormat for OCaml corrections
   and its built-in formatter for ``dune`` and ``dune-project`` corrections.
   Only languages enabled by the source's ``formatting`` configuration are
   formatted. This setting does not affect the ``@fmt`` alias.
