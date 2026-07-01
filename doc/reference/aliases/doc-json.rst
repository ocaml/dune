@doc-json
=========

This alias builds documentation for public libraries as JSON files. These are
produced by ``odoc``'s option ``--as-json`` and can be consumed by external
tools.

The generated files mirror the HTML documentation hierarchy and use the
``.html.json`` suffix. For example, the package index is generated at
``_build/default/_doc/_html/index.html.json``.

.. seealso:: :doc:`/documentation`
