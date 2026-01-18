scope
-----

.. versionadded:: 3.22

The ``scope`` stanza controls which libraries from a subdirectory are visible
to the current project. This is useful when vendoring external projects that
define multiple packages, allowing you to expose only the libraries you need.

.. code:: dune

   (scope
    (dir <directory>)
    (packages <packages>))

``(dir <directory>)`` specifies the immediate subdirectory containing the
libraries. Only direct children of the current directory are allowed; to scope
a nested directory, place the scope stanza in the appropriate parent directory
or use a :doc:`subdir` stanza.

``(packages <packages>)`` uses :doc:`../ordered-set-language` to specify which
packages should be visible. All public libraries (those with a ``public_name``)
from the specified packages become accessible. Libraries without a
``public_name`` are never visible through scope stanzas, as they are project-
internal and not intended for external use.

It is an error to specify a package that does not exist in the scoped directory.

Examples
========

Expose all libraries from a single package:

.. code:: dune

   (scope
    (dir fmt.0.9.0)
    (packages fmt))

Expose libraries from multiple packages in one directory:

.. code:: dune

   (scope
    (dir cohttp.6.0.0)
    (packages cohttp cohttp-lwt cohttp-async))

Expose all packages using ``:standard``:

.. code:: dune

   (scope
    (dir vendor.1.0.0)
    (packages :standard))

Expose all packages except specific ones:

.. code:: dune

   (scope
    (dir cohttp.6.0.0)
    (packages :standard \ cohttp-async))
