package
-------

.. dune:stanza:: package

   Define package-specific metadata.

   .. dune:field:: name
      :param: <string>

      The name of the package.

      This must be specified.

   .. dune:field:: synopsis
      :param: <string>

      A short package description.

   .. dune:field:: description
      :param: <string>

      A longer package description.

   .. dune:field:: depends
      :param: <dep-specification>

      Package dependencies, as :token:`~pkg-dep:dep_specification`.

   .. dune:field:: conflicts
      :param: <dep-specification>

      Package conflicts, as :token:`~pkg-dep:dep_specification`.

   .. dune:field:: depopts
      :param: <dep-specification>

      Optional package dependencies, as :token:`~pkg-dep:dep_specification`.

   .. dune:field:: tags
      :param: <tags>

      A list of tags.

   .. dune:field:: deprecated_package_names
      :param: <name list>

      A list of names that can be used with the :ref:`deprecated-library-name`
      stanza to migrate legacy libraries from other build systems that do not
      follow Dune's convention of prefixing the library's public name with the
      package name.

   .. dune:field:: license

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: authors

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: maintainers

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: source

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: bug_reports

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: homepage

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: documentation

      .. versionadded:: 2.0

      The same as (and takes precedences over) the corresponding global field.

   .. dune:field:: sites

      Define a site.

      ``(sites (<section> <name>) ...)`` defines a site named ``<name>`` in the
      section ``<section>``.

Adding libraries to different packages is done via the ``public_name`` and
``package`` fields. See :ref:`library` section for details.

The list of dependencies :token:`~pkg-dep:dep_specification` is modelled after
opam's own language. The syntax is a list of the following elements:

.. productionlist:: pkg-dep
   op : '=' | '<' | '>' | '<>' | '>=' | '<='
   filter : :dev | :build | :with-test | :with-doc | :post
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
