##############################
 Building an Ad Hoc ``.cmxs``
##############################

..
   TODO(diataxis) howto: Building an Ad Hoc ``.cmxs``

In the model exposed by Dune, a ``.cmxs`` target is created for each
library. However, the ``.cmxs`` format itself is more flexible and is
capable to containing arbitrary ``.cmxa`` and ``.cmx`` files.

For the specific cases where this extra flexibility is needed, one can
use :ref:`variables-for-artifacts` to write explicit rules to build
``.cmxs`` files not associated to any library.

Below is an example where we build ``my.cmxs`` containing ``foo.cmxa``
and ``d.cmx``. Note how we use a :doc:`/reference/files/dune/library`
stanza to set up the compilation of ``d.cmx``.

.. code:: dune

   (library
    (name foo)
    (modules a b c))

   (library
    (name dummy)
    (modules d))

   (rule
    (targets my.cmxs)
    (action (run %{ocamlopt} -shared -o %{targets} %{cmxa:foo} %{cmx:d})))
