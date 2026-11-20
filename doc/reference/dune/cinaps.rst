cinaps
------

A ``cinaps`` stanza is available to support the ``cinaps`` tool.  See the
`cinaps website <https://github.com/janestreet/cinaps>`_ for more details.

With ``(using cinaps 1.4)`` (available since Dune 3.26), generated corrections
are formatted according to the source's formatting configuration before being
compared or promoted. OCaml files use OCamlFormat. Corrections for ``dune`` and
``dune-project`` use Dune's built-in formatter when formatting is enabled for
``dune``. Earlier extension versions retain unformatted corrections.
