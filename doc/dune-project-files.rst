**********************
``dune-project`` files
**********************

These files are used to mark the root of projects as well as define project-wide
parameters. These files are required to have a ``lang`` which controls the names
and contents of all configuration files read by Dune. The ``lang`` stanza looks
like:

.. code:: scheme

          (lang dune 1.0)

Additionally, they can contains the following stanzas.

name
----

Sets the name of the project. This is used by :ref:`dune subst <dune-subst>`
and error messages.

.. code:: scheme

    (name <name>)

version
-------

Sets the version of the project:

.. code:: scheme

    (version <version>)

implicit_transitive_deps
------------------------

Enables or disables :ref:`implicit-transitive-deps`:

.. code:: scheme

    (implicit_transitive_deps <bool>)

wrapped_executables
-------------------

Enables or disables :ref:`wrapped-executables`:

.. code:: scheme

    (wrapped_executables <bool>)

explicit_js_mode
----------------

Enables :ref:`explicit-js-mode`:

.. code:: scheme

    (explicit_js_mode)

dialect
-------

Defines :ref:`dialects-main` for this project:

.. code:: scheme

    (dialect
     (name <name>)
     (implementation
      (extension <string>)
      <optional fields>)
     (interface
      (extension <string>)
      <optional fields>))

``<name>`` is the name of the dialect being defined. It must be unique in a
given project.

``(extension <string>)`` specifies the file extension used for this dialect, for
interfaces and implementations. The extension string must not contain any dots,
and be unique in a given project.

``<optional fields>`` are:

- ``(preprocess <action>)`` is the action to run to produce a valid OCaml
  abstract syntax tree. It is expected to read the file given in the variable
  named ``input-file`` and output a *binary* abstract syntax tree on its
  standard output. See :ref:`preprocessing-actions` for more information.

  If the field is not present, it is assumed that the corresponding source code
  is already valid OCaml code and can be passed to the OCaml compiler as-is.

- ``(format <action>)`` is the action to run to format source code for this
  dialect. The action is expected to read the file given in the variable named
  ``input-file`` and output the formatted source code on its standard
  output. For more information. See :ref:`formatting-main` for more information.

  If the field is not present, then if ``(preprocess <action>)`` is not present
  (so that the dialect consists of valid OCaml code), then by default the
  dialect will be formatted as any other OCaml code. Otherwise no special
  formatting will be done.

generate_opam_files
-------------------

Dune is able to use metadata specified in the ``dune-project`` file to generate
``.opam`` files, see :ref:`opam-generation`. To enable this integration, add the following
field to the ``dune-project`` file:

.. code:: scheme

   (generate_opam_files true)

Dune uses the following global fields to set the metadata for all packages
defined in the project:

- ``(license <name>)`` - Specifies the license of the project, ideally as an
  identifier from the `SPDX License List <https://spdx.org/licenses/>`__

- ``(authors <authors>)`` - A list of authors

- ``(maintainers <maintainers>)`` - A list of maintainers

- ``(source <source>)`` - where the source is specified two ways:
  ``(github <user/repo>)`` or ``(uri <uri>)``

- ``(bug_reports <url>)`` - Where to report bugs. This defaults to the GitHub
  issue tracker if the source is specified as a GitHub repository

- ``(homepage <url>)`` - The homepage of the project

- ``(documentation <url>)`` - Where the documentation is hosted

package
-------

Package specific information is specified in the ``(package <package>)`` stanza.
It contains the following fields:

- ``(name <string>)`` is the name of the package. This must be specified.

- ``(synopsis <string>)`` is a short package description

- ``(description <string>)`` is a longer package description

- ``(depends <dep-specification>)`` are package dependencies

- ``(conflicts <dep-specification)`` are package conflicts

- ``(depopts <dep-specification)`` are optional package dependencies

- ``(tags <tags>)`` are the list of tags for the package

The list of dependencies ``<dep-specification>`` is modeled after opam's own
language: The syntax is as a list of the following elements:

.. code::

   op := '=' | '<' | '>' | '<>' | '>=' | '<='

   stage := :with_test | :build | :dev

   constr := (<op> <version>)

   logop := or | and

   dep := (name <stage>)
        | (name <constr>)
        | (name (<logop> (<stage> | <constr>)*))

   dep-specification = dep+
