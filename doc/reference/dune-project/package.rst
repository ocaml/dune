package
-------

This stanza is used to specify package metadata. In particular, this information
is used when generating OPAM files (see :doc:`generate_opam_files`).

.. describe:: (package ...)

   Define package-specific metadata.

   .. describe:: (name <string>)

      The name of the package.

      This must be specified.

   .. describe:: (synopsis <string>)

      A short package description.

   .. describe:: (description <string>)

      A longer package description.

   .. describe:: (depends <dep-specification>)

      Package dependencies, as :token:`~pkg-dep:dep_specification`.

   .. describe:: (conflicts <dep-specification>)

      Package conflicts, as :token:`~pkg-dep:dep_specification`.

   .. describe:: (depopts <dep-specification>)

      Optional package dependencies, as :token:`~pkg-dep:dep_specification`.

   .. describe:: (tags <tags>)

      A list of tags.

   .. describe:: (deprecated_package_names <name list>)

      A list of names that can be used with the
      :doc:`../dune/deprecated_library_name` stanza to migrate legacy libraries
      from other build systems that do not follow Dune's convention of
      prefixing the library's public name with the package name.

   .. describe:: (license ...)

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field
      (see :doc:`license`).

   .. describe:: (authors ...)

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field
      (see :doc:`authors`).

   .. describe:: (maintainers ...)

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field
      (see :doc:`maintainers`).

   .. describe:: (maintenance_intent ...)

      .. versionadded:: 3.18

      The same as (and takes precedences over) the corresponding global field
      (see :doc:`maintenance_intent`).

   .. describe:: (source ...)

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field
      (see :doc:`source`).

   .. describe:: (bug_reports ...)

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field
      (see :doc:`bug_reports`).

   .. describe:: (homepage ...)

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field
      (see :doc:`homepage`).

   .. describe:: (documentation ...)

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field
      (see :doc:`documentation`).

   .. describe:: (sites ...)

      Define a site.

      ``(sites (<section> <name>) ...)`` defines a site named ``<name>`` in the
      section ``<section>``.

Adding libraries to different packages is done via the ``public_name`` and
``package`` fields. See :doc:`../dune/library` section for details.

The list of dependencies :token:`~pkg-dep:dep_specification` is modelled after
opam's own language. The syntax is a list of the following elements:

.. productionlist:: pkg-dep
   op : '=' | '<' | '>' | '<>' | '>=' | '<='
   filter : :dev | :build | :with-test | :with-doc | :with-dev-setup | :post
   constr : (<op> <version>)
   logop : or | and
   dep : <name>
       : (<name> <filter>)
       : (<name> <constr>)
       : (<name> (<logop> (<filter> | <constr>))*)
   dep_specification : <dep>+

Filters will expand to any opam variable name if prefixed by ``:``, not just the
ones listed in :token:`~pkg-dep:filter`. This also applies to version numbers.
For example, to generate ``depends: [ pkg { = version } ]``, use ``(depends
(pkg (= :version)))``.

Note that the use of a ``using`` stanza (see :doc:`using`) doesn't
automatically add the associated library or tool as a dependency. They have to
be added explicitly.
