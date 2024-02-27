#################
 Package Version
#################

..
   TODO(diataxis)
   - reference: environment - packages

Dune determines a package's version by looking at the ``version`` field
in the :doc:`/reference/files/dune-project/package`. If the version
field isn't set, it looks at the toplevel ``version`` field in the
``dune-project`` field. If neither are set, Dune assumes that we are in
development mode and reads the version from the VCS, if any. The way it
obtains the version from the VCS is described in :ref:`the build-info
section <build-info>`.

When installing the files of a package on the system, Dune automatically
inserts the package version into various metadata files such as ``META``
and ``dune-package`` files.
