How to Bundle Resources
=======================

This guide will show you how to configure Dune to generate modules with string resources
from other files in your project.

Folder Structure
----------------

.. code:: bash

    $ tree src
    src
    └── lib
        └── my_lib
            ├── dune
            └── resources
                └── site.css

Dune Configuration
------------------

See the section on :doc:`../reference/actions` for more details on :dune:ref:`action-progn` and :dune:ref:`action-with-<outputs>-to`.

.. code:: dune

    (rule
     (with-stdout-to
      css.ml
      (progn
       (echo "let css = {|")
       (cat resources/site.css)
       (echo "|}"))))

Using the Bundled Resource
--------------------------

.. code:: ocaml

   let () = Printf.printf "%s" Css.css
