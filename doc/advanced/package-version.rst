Package Version
===============

.. TODO(diataxis)
   - reference: environment - packages

Dune records package versions in generated metadata such as generated opam
files, ``META`` files, and ``dune-package`` files. The same version is
available in Dune files with the ``%{version:<package>}`` variable.

For packages declared with a :doc:`/reference/dune-project/package` stanza,
Dune determines the version in this order:

1. The package-specific ``version`` field in the ``(package ...)`` stanza.
2. The top-level :doc:`/reference/dune-project/version` field in
   ``dune-project``.
3. No version.

When a package is declared in ``dune-project``, Dune does not read the version
from an existing ``<package>.opam`` file for generated metadata. If you want
Dune to manage opam files, use
:doc:`/reference/dune-project/generate_opam_files` and keep the version in
``dune-project``.

For projects without any ``(package ...)`` stanzas, Dune can infer packages
from existing opam files. In that case, the version from ``<package>.opam`` is
used first; if the opam file has no version, Dune falls back to the top-level
``(version ...)`` field in ``dune-project``.

Ordinary builds do not use the VCS version as a fallback for generated package
metadata. VCS-derived versions are used by ``dune subst`` and by the
``dune-build-info`` library. The way Dune obtains the version from the VCS is
described in :ref:`the build-info section <build-info>`.
