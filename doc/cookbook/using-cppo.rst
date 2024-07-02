Using Cppo
==========

Add this field to your ``library`` or ``executable`` stanzas:

.. code:: dune

    (preprocess (action (run %{bin:cppo} -V OCAML:%{ocaml_version} %{input-file})))

Additionally, if you want to include a ``config.h`` file, you need to
declare the dependency to this file via:

.. code:: dune

    (preprocessor_deps config.h)

Using the ``.cppo.ml`` Style Like the ``ocamlbuild`` Plugin
-----------------------------------------------------------

Write this in your ``dune`` file:

.. code:: dune

    (rule
     (targets foo.ml)
     (deps    (:first-dep foo.cppo.ml) <other files that foo.ml includes>)
     (action  (run %{bin:cppo} %{first-dep} -o %{targets})))
