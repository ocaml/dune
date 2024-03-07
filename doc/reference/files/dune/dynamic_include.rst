dynamic_include
---------------

The ``dynamic_include`` stanza allows including the contents of another file in
the current dune file like the ``include`` stanza.  However, the
``dynamic_include`` stanza allows the included file to be the target of a rule
and disallows generating some stanzas.

For instance:

.. code:: dune

   (subdir b
    (dynamic_include ../a/foo.inc))
   (subdir a
    (rule
     (write-file
      foo.inc
      "(rule (write-file file bar))")))

In the example above, the dynamic rule loading and generation are split into
different directories to avoid rule loading cycles as rules are loaded per
directory.

The following stanzas cannot be dynamically generated:

* Libraries, coq theories, library redirects
* Public executables or install section with the ``bin`` section
* Plugin stanzas
