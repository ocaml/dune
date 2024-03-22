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
