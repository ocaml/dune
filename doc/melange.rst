.. _melange_main:

***********************************
JavaScript Compilation With Melange
***********************************

Introduction
============

`Melange <https://github.com/melange-re/melange>`_ compiles OCaml to
JavaScript. It produces one JavaScript file per OCaml module. Melange can
be installed with opam:

.. code:: console

   $ opam install melange

Dune can build Melange projects, and produces JavaScript files by defining a
:ref:`melange-emit` stanza. Dune libraries may also define Melange libraries by
adding ``melange`` to ``(modes ...)`` in the :doc:`/reference/dune/library`
stanza.

Melange support is enabled by default in Dune language 3.25 and later. Projects
using older Dune language versions must enable it in the
:doc:`/reference/dune-project/index` file:

.. code:: dune

    (using melange 1.0)

Once Melange support is enabled, you can use the Melange mode in
:doc:`/reference/dune/library` and ``melange.emit`` stanzas.

Simple Project
==============

Let's start by looking at a simple project with Melange and Dune. Subsequent
sections explain the different concepts used here in further detail.

First, make sure that the :doc:`/reference/dune-project/index` file specifies
at least version 3.25 of the Dune language:

.. code:: dune

  (lang dune {{latest}})

Next, write a :doc:`/reference/dune/index` file with a
:ref:`melange-emit` stanza:

.. code:: dune

  (melange.emit
   (target output))

Finally, add a source file to build:

.. code:: console

  $ echo 'Js.log "hello from melange"' > hello.ml

After running ``dune build @melange`` or just ``dune build``, Dune
produces the following file structure:

.. code::

  .
  ├── _build
  │   └── default
  │       └── output
  │           └── hello.js
  ├── dune
  ├── dune-project
  └── hello.ml

The resulting JavaScript can now be run:

.. code:: console

   $ node _build/default/output/hello.js
   hello from melange


Libraries
=========

Dune libraries can be compiled as OCaml libraries, Melange libraries, or both.
The selected modes control which compiler is used, and the Melange-specific
library fields let one stanza describe sources, dependencies, and preprocessing
that differ between OCaml and Melange builds.

Certain Melange-only fields are only available from Dune 3.24:

.. code:: dune

  (lang dune 3.24)
  (using melange 1.0)

Choosing Library Modes
----------------------

The ``modes`` field of the :doc:`/reference/dune/library` stanza decides which
library variants Dune builds:

- ``(modes melange)`` builds only the Melange variant of the library.

- ``(modes :standard melange)`` builds the usual OCaml variants from
  ``:standard`` and also builds a Melange variant. This is the common choice
  when one library is shared by native OCaml code and Melange code.

- ``(modes byte)`` or ``(modes :standard)`` builds only OCaml variants. Such a
  library cannot use Melange-only library fields such as
  ``melange.libraries`` and cannot be used by a ``melange.emit`` stanza.

The ``modes`` field supports the :doc:`/reference/ordered-set-language`, so
``melange`` can be added to or removed from ``:standard`` in the same way as
other modes.

Selecting Melange Sources
-------------------------

By default, a mixed-mode library uses the same module set for OCaml and Melange
compilation. Dune also recognizes Melange-specific source files:

- ``foo.melange.ml`` replaces ``foo.ml`` when compiling module ``Foo`` in
  Melange mode.

- ``foo.melange.mli`` replaces ``foo.mli`` when compiling module ``Foo``'s
  interface in Melange mode.

- Reason sources work the same way: ``foo.melange.re`` and
  ``foo.melange.rei`` replace ``foo.re`` and ``foo.rei`` in Melange mode.

- A ``*.melange.ml`` or ``*.melange.re`` file can also define a module that
  exists only in the Melange variant, as long as that module is part of the
  Melange module set.

For example, this library builds ``Shared`` in both modes, but uses
``Override.ml`` for OCaml and ``Override.melange.ml`` for Melange:

.. code:: dune

  (library
   (name shared_with_override)
   (modes :standard melange)
   (modules shared override))

If the module set itself differs between OCaml and Melange, use
``melange.modules``:

.. versionadded:: 3.24

.. code:: dune

  (library
   (name platform_code)
   (modes :standard melange)
   (modules common ocaml_extra)
   (melange.modules common melange_extra))

In this example, the OCaml variants contain ``Common`` and ``Ocaml_extra``.
The Melange variant contains ``Common`` and ``Melange_extra``.

``melange.modules`` uses the same
:doc:`/reference/ordered-set-language` as ``modules``. If it is omitted,
Melange compilation uses the ``modules`` field.

Mode-Specific Dependencies
--------------------------

By default, the Melange variant of a library uses the same ``libraries`` field
as the OCaml variants. Use ``melange.libraries`` when the dependencies differ:

.. versionadded:: 3.24

.. code:: dune

  (library
   (name app)
   (modes :standard melange)
   (libraries native_dep)
   (melange.libraries melange_dep))

In this example, bytecode and native compilation depend on ``native_dep``.
Melange compilation depends on ``melange_dep`` instead.

``melange.libraries`` replaces ``libraries`` for Melange compilation. It can be
empty, which means the Melange variant has no library dependencies even if the
OCaml variants do.

For PPX rewriters with runtime dependencies that differ between OCaml and
Melange, use ``melange.ppx_runtime_libraries``:

.. versionadded:: 3.24

.. code:: dune

  (library
   (name my_ppx)
   (kind ppx_rewriter)
   (ppx_runtime_libraries my_ppx.native_runtime)
   (melange.ppx_runtime_libraries my_ppx.melange_runtime))

If ``melange.ppx_runtime_libraries`` is omitted, Melange uses
``ppx_runtime_libraries``.

Mode-Specific Preprocessing
---------------------------

By default, the Melange variant of a library uses the same ``preprocess`` field
as the OCaml variants. Use ``melange.preprocess`` when Melange needs different
preprocessing:

.. versionadded:: 3.24

.. code:: dune

  (library
   (name portable_ui)
   (modes :standard melange)
   (modules shared components)
   (preprocess
    (action
     (run sh %{dep:pp_ocaml.sh} %{input-file})))
   (melange.preprocess
    (pps melange.ppx)))

``melange.preprocess`` replaces ``preprocess`` for Melange compilation. It is
not an addition to ``preprocess``. If the Melange preprocessor reads extra
files, list them with ``melange.preprocessor_deps``:

.. versionadded:: 3.24

.. code:: dune

  (library
   (name generated)
   (modes :standard melange)
   (melange.preprocess
    (action
     (run ./pp.sh %{input-file})))
   (melange.preprocessor_deps pp.sh))

Melange Compile Flags and Runtime Dependencies
----------------------------------------------

``melange.compile_flags`` passes flags to ``melc`` for the Melange variant of a
library:

.. code:: dune

  (library
   (name warning_policy)
   (modes :standard melange)
   (melange.compile_flags :standard -w +a-70))

The field uses the :doc:`/reference/ordered-set-language`, supports
``(:include ...)`` forms, and can also be set from ``env`` stanzas. Prefer
``:standard`` when adding flags so Dune's default flags are preserved.

``melange.runtime_deps`` declares files that are needed at runtime when a
``melange.emit`` stanza depends on the library. The field is analogous to
``runtime_deps`` in ``melange.emit`` stanzas:

.. code:: dune

  (library
   (name components)
   (public_name my_package.components)
   (modes melange)
   (melange.runtime_deps ./style.css ./assets/logo.svg))

Runtime dependencies can include assets such as CSS, images, fonts, or
JavaScript files. They use the formats described in
:doc:`/concepts/dependency-spec`.

Putting the Pieces Together
---------------------------

This example combines the common patterns for a library shared by OCaml and
Melange builds:

.. code:: dune

  (library
   (name editor_mode_demo)
   (modes :standard melange)
   (modules common override dep_user)
   (melange.modules common override melange_extra dep_user)
   (libraries native_dep)
   (melange.libraries melange_dep)
   (preprocess
    (action
     (run sh %{dep:pp_ocaml.sh} %{input-file})))
   (melange.preprocess
    (action
     (run sh %{dep:pp_melange.sh} %{input-file}))))

With these source files:

.. code::

  common.ml
  override.ml
  override.melange.ml
  dep_user.ml
  melange_extra.melange.ml

OCaml compilation uses ``common.ml``, ``override.ml``, and ``dep_user.ml`` with
``native_dep`` and ``pp_ocaml.sh``. Melange compilation uses ``common.ml``,
``override.melange.ml``, ``dep_user.ml``, and
``melange_extra.melange.ml`` with ``melange_dep`` and ``pp_melange.sh``.

``melange.emit`` can then depend on the library:

.. code:: dune

  (melange.emit
   (target output)
   (libraries editor_mode_demo)
   (modules app))

Building Without Melange
------------------------

.. versionadded:: 3.25

A library can support Melange without requiring it in every opam switch.
Declare both OCaml and Melange modes, for example:

.. code:: dune

  (library
   (public_name portable)
   (modes :standard melange))

If Dune cannot find the Melange compiler, ``melc``, in the build context, it
skips the Melange variant of this library. The OCaml variants can still be
built.

When ``melc`` is available, the library retains both OCaml and Melange modes.
Libraries installed without that variant must be rebuilt and reinstalled with
``melc`` available before they can be used by Melange consumers.

This behavior is available with Dune 3.25 and later.

There are two important limits:

- A library with only ``(modes melange)`` still requires ``melc``; it is not
  silently skipped when the compiler is missing.
- Building the outputs of a ``melange.emit`` stanza still requires ``melc``.
  This includes ``@all`` when it selects those outputs. Merely declaring the
  stanza does not prevent building an unrelated OCaml target without Melange.

The OCaml variants must still have all their dependencies and preprocessors
available. This behavior does not make an unconditional ``melange`` dependency
in an opam package optional.

.. _melange-emit:

melange.emit
============

.. versionadded:: 3.8

The ``melange.emit`` stanza produces JavaScript files from Melange libraries or
entry-point modules. It's similar to the OCaml
:doc:`/reference/dune/executable` stanza, with the exception that there is no
linking step.

.. code:: dune

    (melange.emit
     (target <target>)
     <optional-fields>)

.. _target:

- ``<target>`` is the name of the folder inside the build directory where Dune
  will compile the resulting JavaScript. In particular, the folder will be
  placed under ``_build/default/$path-to-directory-of-melange-emit-stanza``.

    **Note:** when using `promotion`_, Dune will additionally copy the
    resulting JavaScript back to the source tree, next to the original source 
    files.

``$path-to-directory-of-melange-emit-stanza`` matches the file structure of the
source tree. For example, given the following source tree:

.. code::

    ├── dune # (melange.emit (target output) (libraries lib))
    ├── app.ml
    └── lib
        ├── dune # (library (name lib) (modes melange))
        └── helper.ml

The resulting layout in ``_build/default/output`` will be as follows:

.. code::

    output
    ├── app.js
    └── lib
        ├── lib.js
        └── helper.js

.. _melange-emit-artifact-variable:

Artifact Variable
-----------------

.. versionadded:: 3.25

The ``%{melange.emit:<target-dir>}`` variable expands to the output directory
of the selected ``melange.emit`` stanza. ``<target-dir>`` is the path to the
stanza's target directory, relative to the ``dune`` file containing the
variable. Like other :ref:`artifact variables <variables-for-artifacts>`, it
adds a dependency on the stanza's outputs.

For example, suppose ``lib/dune`` contains a stanza with ``(target output)``.
In that file, ``%{melange.emit:output}`` expands to ``output/lib``. In a
``dune`` file at the workspace root, ``%{melange.emit:lib/output}`` expands to
``lib/output/lib``.

``<optional-fields>`` are:

- ``(alias <alias-name>)`` specifies an alias to which to attach the targets of
  the ``melange.emit`` stanza.

  - These targets include the ``.js`` files generated by the stanza
    modules, the targets for the ``.js`` files of any library that the stanza
    depends on, and any copy rules for runtime dependencies (see
    ``runtime_deps`` field below).

  - By default, all stanzas will have their targets attached to an alias
    ``melange``. The behavior of this default alias is exclusive: if an alias
    is explicitly defined in the stanza, the targets from this stanza will
    be excluded from the ``melange`` alias.

  - The targets of ``melange.emit`` are also attached to the Dune default
    alias (:doc:`/reference/aliases/all`), regardless of whether the
    ``(alias ...)`` field is present.

- ``(module_systems <module_systems>)`` specifies the JavaScript import and
  export format used. The values allowed for ``<module_systems>`` are ``es6``
  and ``commonjs``.

  - ``es6`` will follow `JavaScript modules <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules>`_,
    and will produce ``import`` and ``export`` statements.

  - ``commonjs`` will follow `CommonJS modules <https://nodejs.org/api/modules.html>`_,
    and will produce `require` calls and export values with ``module.exports``.

  - If no extension is specified, the resulting JavaScript files will use
    ``.js``. You can specify a different extension with a pair
    ``(<module_system> <extension>)``, e.g. ``(module_systems (es6 mjs))``.

  - Multiple module systems can be used in the same field as long as their
    extensions are different. For example,
    ``(module_systems commonjs (es6 mjs))`` will produce one set of JavaScript
    files using CommonJS and the ``.js`` extension, and another using ES6 and
    the ``.mjs`` extension.

- ``(modules <modules>)`` specifies what modules will be built with Melange. By
  default, if this field is not defined, Dune will use all the ``.ml/.re`` files
  in the same directory as the ``dune`` file. This includes module sources
  present in the file system as well as modules generated by user rules. You can
  restrict this list by using an explicit ``(modules <modules>)`` field.
  ``<modules>`` uses the :doc:`reference/ordered-set-language`, where elements
  are module names and don't need to start with an uppercase letter. For
  instance, to exclude module ``Foo``, use ``(modules :standard \ foo)``.

- ``(libraries <library-dependencies>)`` specifies Melange library dependencies.
  Melange libraries can only use the simple form, like
  ``(libraries foo pkg.bar)``. Keep in mind the following limitations:

  - The ``re_export`` form is not supported.

  - All the libraries included in ``<library-dependencies>`` have to support
    the ``melange`` mode (see the section about libraries below).


- ``(package <package>)`` allows the user to define the JavaScript package to
  which the artifacts produced by the ``melange.emit`` stanza will belong.

- ``(runtime_deps <paths-to-deps>)`` specifies dependencies that should be
  copied to the build folder together with the ``.js`` files generated from the
  sources. These runtime dependencies can include assets like CSS files, images,
  fonts, external JavaScript files, etc. ``runtime_deps`` adhere to the formats
  in :doc:`concepts/dependency-spec`. For example
  ``(runtime_deps ./path/to/file.css (glob_files_rec ./fonts/*))``.

- ``(emit_stdlib <bool>)`` allows the user to specify whether the Melange
  standard library should be included as a dependency of the stanza or not. The
  default is ``true``. If this option is ``false``, the Melange standard library
  and runtime JavaScript files won't be produced in the target directory.

.. _melange_promote:

- ``(promote <options>)`` promotes the generated ``.js`` files to the
  source tree. The options are the same as for the
  :ref:`rule promote mode <promote>`.
  Adding ``(promote (until-clean))`` to a ``melange.emit`` stanza will cause
  Dune to copy the ``.js`` files to the source tree and ``dune clean`` to
  delete them.
  Check `Promotion`_ for more details.

- ``(preprocess <preprocess-spec>)`` specifies how to preprocess files when
  needed. The default is ``no_preprocessing``. Additional options are described
  in the :doc:`reference/preprocessing-spec` section.

- ``(lint <preprocess-spec>)`` specifies how to lint source files when building
  the :doc:`reference/aliases/lint` alias. The default is
  ``no_preprocessing``. The syntax is described in :ref:`lint-field`.

- ``(preprocessor_deps (<deps-conf list>))`` specifies extra preprocessor
  dependencies, e.g., if the preprocessor reads a generated file.
  The dependency specification is described in the :doc:`concepts/dependency-spec`
  section.

- ``(compile_flags <flags>)`` specifies compilation flags specific to
  ``melc``, the main Melange executable.
  ``<flags>`` is described in detail in the
  :doc:`reference/ordered-set-language` section. It also supports
  ``(:include ...)`` forms. The value for this field can also be taken
  from ``env`` stanzas. It's therefore recommended to add flags
  with e.g. ``(compile_flags :standard <my options>)`` rather than
  replace them.

- ``(root_module <module>)`` specifies a ``root_module`` that collects all
  listed dependencies in ``libraries``. See the documentation for
  ``root_module`` in the :doc:`/reference/dune/library` stanza.

- ``(allow_overlapping_dependencies)`` is the same as the corresponding field
  of :doc:`/reference/dune/library`.

- ``(enabled_if <blang expression>)`` conditionally disables a melange emit
  stanza. The JavaScript files associated with the stanza won't be built. The
  condition is specified using the :doc:`reference/boolean-language`.

Recommended Practices
=====================

Keep Bundles Small by Reducing the Number of ``melange.emit`` Stanzas
---------------------------------------------------------------------

It is recommended to minimize the number of ``melange.emit`` stanzas
that a project defines: using multiple ``melange.emit`` stanzas will cause
multiple copies of the JavaScript files to be generated if the same libraries
are used across them. As an example:

.. code:: dune

  (melange.emit
   (target app1)
   (libraries foo))

  (melange.emit
   (target app2)
   (libraries foo))

The JavaScript artifacts for library ``foo`` will be emitted twice in the
``_build`` folder. They will be present under ``_build/default/app1``
and ``_build/default/app2``.

This can have unexpected impact on bundle size when using tools like Webpack or
Esbuild, as these tools will not be able to see shared library code as such,
as it would be replicated across the paths of the different stanzas
``target`` folders.


Faster Builds With ``subdir`` and ``dirs`` Stanzas
--------------------------------------------------

Melange libraries might be installed from the ``npm`` package repository,
together with other JavaScript packages. To avoid having Dune inspect
unnecessary folders in ``node_modules``, it is recommended to explicitly
include only the folders that are relevant for Melange builds.

This can be accomplished by combining :doc:`/reference/dune/subdir` and
:doc:`/reference/dune/dirs` stanzas in a ``dune`` file next to the
``node_modules`` folder. The :doc:`/reference/dune/vendored_dirs` stanza
can be used to avoid warnings in Melange libraries during the application
build. The :doc:`/reference/dune/data_only_dirs` stanza can be useful as
well if you need to override the build rules in one of the packages.

.. code:: dune

  (subdir
   node_modules
   (vendored_dirs reason-react)
   (dirs reason-react))

Promotion
=====================

Compiling and promoting Melange output in Dune is slightly different than
compiling OCaml:

- Limitations in Dune `rule production
  <https://github.com/ocaml/dune/blob/main/doc/dev/rule-streaming.md>`_ require
  a :ref:`target directory <target>` in :ref:`melange-emit`.

  - The target directory is :ref:`total <total>`: it can be exported as is from
    the Dune build directory
- Many popular tools and frameworks in the JavaScript ecosystem today rely on
  convention over configuration, especially as it relates to folder structure.
  When using :ref:`promotion <melange_promote>`



Design choices
=====================

Melange support in Dune follows the following design choices:

.. _total:

- :ref:`melange-emit` produces a "total" directory: the artifacts in the
  ``target`` directory contain all the JavaScript and ``runtime_deps`` assets
  necessary to run the application either through a JS framework, a bundler, or
  otherwise a deployment (excluding external dependencies installed via a JS
  package manager). The structure is designed such that relative paths and
  dependencies work out of the box relative to their paths in the source tree,
  before compilation.
- public libraries are compiled to ``%{target}/node_modules/%{lib_name}`` such
  that the `resolution algorithm
  <https://nodejs.org/api/modules.html#all-together>`_ works to resolve Melange
  libraries from compiled JS code.
- JavaScript output is promoted to the source tree 
