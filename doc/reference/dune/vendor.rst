vendor
------

.. versionadded:: 3.22

The ``vendor`` stanza filters which libraries from a vendored directory are
exposed to the project. It supports library aliasing to allow multiple versions
of the same library to coexist.

.. code:: dune

   (vendor <directory>
    (package <name>)
    (libraries <lib-spec>))

``<directory>`` is the path to the vendored directory, relative to the ``dune``
file. All fields except the directory are optional.

Example with a cohttp-like structure (multiple packages in one directory):

.. code:: dune

   (vendor cohttp.6.0.0 (package cohttp))
   (vendor cohttp.6.0.0 (package cohttp-lwt))
   (vendor cohttp.6.0.0 (package cohttp-eio))

The ``package`` field restricts the stanza to libraries belonging to that
package. Multiple vendor stanzas can reference the same directory, each for a
different package.

The ``libraries`` field uses the :doc:`/reference/ordered-set-language`:

.. code:: dune

   (vendor cohttp.6.0.0 (package cohttp) (libraries cohttp cohttp-lwt))
   (vendor cohttp.6.0.0 (package cohttp) (libraries :standard \ cohttp-bench))

Library Aliasing
~~~~~~~~~~~~~~~~

Libraries can be exposed under a different name using ``:as``:

.. code:: dune

   (vendor yojson.1.0.0 (libraries (yojson :as yojson_v1)))

The alias renames the wrapper module. When the library name equals the entry
module name, they normally collapse into one; with aliasing they become distinct:

.. code:: ocaml

   Yojson.Basic.from_string "{}"            (* without alias *)
   Yojson_v1.Yojson.Basic.from_string "{}"  (* with alias *)

This allows vendoring multiple versions of the same library:

.. code:: dune

   (vendor yojson.1.7.0 (libraries (yojson :as yojson_v1)))
   (vendor yojson.2.0.0 (libraries yojson))

Multiple Versions
~~~~~~~~~~~~~~~~~

When the same repository contains multiple packages, you can vendor different
versions and cherry-pick specific packages from each:

.. code::

   vendor/
     cohttp.6.0.0/
     cohttp.6.1.0/
     dune

In ``vendor/dune``, use ``cohttp`` from 6.0.0 but ``cohttp-lwt`` from 6.1.0:

.. code:: dune

   (vendor cohttp.6.0.0 (package cohttp))
   (vendor cohttp.6.1.0 (package cohttp-lwt))

Interaction with vendored_dirs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``vendor`` and :doc:`vendored_dirs` stanzas can be combined. ``vendored_dirs``
suppresses warnings and disables aliases; ``vendor`` controls library exposure.
Using both gives warning suppression with library filtering:

.. code:: dune

   (vendored_dirs cohttp.6.0.0)
   (vendor cohttp.6.0.0 (package cohttp))
   (vendor cohttp.6.0.0 (package cohttp-lwt))

See Also
~~~~~~~~

- :doc:`vendored_dirs`
- :doc:`/reference/ordered-set-language`
