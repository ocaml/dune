@fmt
====

This alias is used by formatting rules: when it is built, code formatters will
be executed (using :doc:`promotion </concepts/promotion>`).

``dune fmt`` is a shortcut for ``dune build @fmt --auto-promote``.

It is possible to build on top of this convention. If some actions are manually
attached to the ``fmt`` alias, they will be executed by ``dune fmt``.

Example:

.. code:: dune

   (rule
    (with-stdout-to
     data.json.formatted
     (run jq . %{dep:data.json})))

   (rule
    (alias fmt)
    (action
     (diff data.json data.json.formatted)))
