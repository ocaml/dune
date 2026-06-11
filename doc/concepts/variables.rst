Variables
=========

.. TODO(diataxis)
   - reference: variables
   - explanation: rule loading

Some fields can contains variables that are expanded by Dune.
The syntax of variables is as follows:

.. code::

   %{var}

or, for more complex forms that take an argument:

.. code::

   %{fun:arg}

In order to write a plain ``%{``, you need to write ``\%{`` in a
string.

Unless otherwise noted, path variables in actions refer to paths in the current
build context and are rendered relative to the action's current working
directory. By default, an action in ``src/foo/dune`` runs from
``_build/default/src/foo``, so ``%{workspace_root}`` may be rendered as
``../..``. These variables do not point directly to your source checkout. If an
action needs to read a source file, declare the file as a dependency and refer
to it with a dependency variable such as ``%{dep:path/to/file}``. This lets
Dune track the dependency and make it available to sandboxed actions.

Dune supports the following variables:

- ``project_root`` is the root of the current project in the current build
  context. It is typically the root of your project under ``_build/<context>``,
  and as long as you have a ``dune-project`` file there, ``project_root`` is
  independent of the workspace configuration.
- ``workspace_root`` is the root of the current workspace in the current build
  context. For the default build context this is typically ``_build/default``.
  Note that the value of ``workspace_root`` isn't constant and depends on
  whether your project is vendored or not.
-  ``cc`` is the C compiler command line (list made of the compiler
   name followed by its flags) that will be used to compile foreign code. For
   more details about its content, please see :doc:`/reference/foreign-flags`.
-  ``cxx`` is the C++ compiler command line being used in the
   current build context.
-  ``ocaml_bin`` is the path where ``ocamlc`` lives.
-  ``ocaml`` is the ``ocaml`` binary.
-  ``ocamlc`` is the ``ocamlc`` binary.
-  ``ocamlopt`` is the ``ocamlopt`` binary.
-  ``ocaml_version`` is the version of the compiler used in the
   current build context.
-  ``ocaml_where`` is the output of ``ocamlc -where``.
-  ``arch_sixtyfour`` is ``true`` if using a compiler that targets a
   64-bit architecture and ``false`` otherwise.
-  ``null`` is ``/dev/null`` on Unix or ``nul`` on Windows.
-  ``ext_obj``, ``ext_asm``, ``ext_lib``, ``ext_dll``, and ``ext_exe``
   are the file extensions used for various artifacts.
- ``ext_plugin`` is ``.cmxs`` if ``natdynlink`` is supported and
  ``.cma`` otherwise.
- ``ocaml-config:v`` expands to the output of ``ocamlc -config-var v``
  for every configuration variable ``v``. Note that Dune processes the
  output of ``ocamlc -config`` in order to make it a bit more stable
  across versions, so the exact set of variables accessible this way
  might not be exactly the same as what you can see in the output of
  ``ocamlc -config``. In particular, variables added in new OCaml
  versions need to be registered in Dune before they can be used.
- ``profile`` is the profile selected via ``--profile``.
- ``context_name`` is the name of the context (``default``, or defined in the
  workspace file)
- ``os_type`` is the type of the OS the build is targeting. This is
  the same as ``ocaml-config:os_type``.
- ``architecture`` is the type of the architecture the build is targeting. This
  is the same as ``ocaml-config:architecture``.
- ``model`` is the type of the CPU the build is targeting. This is
  the same as ``ocaml-config:model``.
- ``system`` is the name of the OS the build is targeting. This is the same as
  ``ocaml-config:system``.

The target values of ``os_type``, ``architecture``, ``model``, and ``system``
come from the OCaml compiler configuration used by the build context. For
example, to inspect the values for the current context, run:

.. code:: console

   $ dune exec -- ocamlc -config-var os_type
   $ dune exec -- ocamlc -config-var architecture
   $ dune exec -- ocamlc -config-var model
   $ dune exec -- ocamlc -config-var system

On a typical Linux switch these may print ``Unix``, ``x86_64`` or ``arm64``,
``default``, and ``linux`` respectively. When writing Dune files, the
corresponding ``%{ocaml-config:...}`` variables expand to the same values.

- ``ignoring_promoted_rules`` is ``true`` if
  ``--ignore-promoted-rules`` was passed on the command line and
  ``false`` otherwise.
- ``dune-warnings`` is the list of OCaml warnings that Dune used by default up
  until version 3.20 of the Dune language when building in the ``dev`` profile.
  This was a larger set of warnings than the default one used by the OCaml
  compiler, and in version 3.21 of the Dune language the set of warnings used by
  the ``dev`` profile was reverted to the default one used by the compiler. This
  variable is made available for those users who would like to keep using Dune's
  stricter warning set. The old behaviour of Dune can be recovered by using the
  following stanza in a top-level ``dune`` file: ``(env (dev (flags :standard
  %{dune-warnings})))``.
- ``git-sha`` expands to the short git SHA of the HEAD commit of the workspace's
  git repository (equivalent to ``git rev-parse --short HEAD``). Expands to the
  empty string when no commit sha was found. Available since Dune 3.24.
- ``<ext>:<path>`` where ``<ext>`` is one of ``cmo``, ``cmi``, ``cma``,
  ``cmx``, or ``cmxa``. See :ref:`variables-for-artifacts`.
- ``env:<var>=<default`` expands to the value of the environment
  variable ``<var>``, or ``<default>`` if it does not exist.
  For example, ``%{env:BIN=/usr/bin}``.
  Available since Dune 1.4.0.
- There are some Rocq-specific variables detailed in :ref:`rocq-variables`.

In addition, ``(action ...)`` fields support the following special variables:

- ``target`` expands to the one target.
- ``targets`` expands to the list of target.
- ``deps`` expands to the list of dependencies.
- ``^`` expands to the list of dependencies, separated by spaces.
- ``dep:<path>`` expands to ``<path>`` (and adds ``<path>`` as a dependency of
  the action).
- ``exe:<path>`` expands to an executable target in the source tree and adds it
  as a dependency of the action. Use this form to run an in-tree executable by
  path, for example ``%{exe:./tool.exe}`` or ``%{exe:../bin/tool.exe}``. The
  executable does not need a ``public_name`` and does not need to be installed.
  This is similar to ``dep:<path>``, except that Dune will map the executable to
  a version that can run on the build machine when cross-compiling.
- ``bin:<program>`` expands to a runnable path for ``program`` and adds
  it as a dependency of the action. If ``<program>`` is the public name
  of an executable in the workspace, the expansion is the build-artifact
  path of the locally built binary; otherwise, ``<program>`` is looked
  up in the build context's ``PATH`` and the expansion is its absolute
  path. When the resulting relative path is a bare basename (i.e. the
  binary lives in the action's directory), Dune prepends ``./`` so that
  shells like ``bash`` execute the file directly rather than performing
  a ``PATH`` lookup.

  When ``%{bin:<program>}`` appears in ``(deps ...)``, the action
  additionally gets an isolated directory prepended to ``PATH``,
  containing a symlink named ``<program>`` (without the artifact
  extension) for each declared ``%{bin:...}`` dep. This lets the binary
  be invoked by its bare name from ``(bash ...)`` or ``(system ...)``.

  ``%{bin:...}`` is not required with ``(run ...)``: ``(run %{bin:foo}
  ...)`` and ``(run foo ...)`` behave the same.
- ``bin-available:<program>`` expands to ``true`` or ``false``, depending
  on whether ``<program>`` is available or not.
- ``file-available:<path>`` expands to ``true`` or ``false``, depending on
  whether the file at ``<path>`` is available in the current workspace.
- ``lib:<public-library-name>:<file>`` expands to the file's installation path
  ``<file>`` in the library ``<public-library-name>``. If
  ``<public-library-name>`` is available in the current workspace, the local
  file will be used, otherwise the one from the :term:`installed world` will be
  used.
- ``lib-private:<library-name>:<file>`` expands to the file's build path
  ``<file>`` in the library ``<library-name>``. Both public and private library
  names are allowed as long as they refer to libraries within the same project.
- ``libexec:<public-library-name>:<file>`` is the same as ``lib:...``, except
  when cross-compiling, in which case it will expand to the file from the host
  build context.
- ``libexec-private:<library-name>:<file>`` is the same as ``lib-private:...``
  except when cross-compiling, in which case it will expand to the file from the
  host build context.
- ``lib-available:<library-name>`` expands to ``true`` or ``false`` depending on
  whether the library is available or not. A library is available if at least
  one of the following conditions holds:

  -  It's part the :term:`installed world`.
  -  It's available locally and is not optional.
  -  It's available locally, and all its library dependencies are
     available.

- ``version:<package>`` expands to the version of the given
  package. Packages defined in the current scope have priority over the
  public packages. Public packages that don't install any libraries
  will not be detected. How Dune determines the version
  of a package is described :doc:`here <../advanced/package-version>`.
- ``read:<path>`` expands to the contents of the given file.
- ``read-lines:<path>`` expands to the list of lines in the given
  file.
- ``read-strings:<path>`` expands to the list of lines in the given
  file, unescaped using OCaml lexical convention.
- ``ppx:lib1+..+libn`` expands to the ppx executable with ppx libraries
  ``lib1`` to ``libn`` linked in. This form also introduces a dependency on
  this executable.
- ``pkg:<package>:<section>:<path>`` expands to the path of a file
  installed by ``<package>`` in ``<section>`` at the relative ``<path>``
  within that section. Works with workspace packages, lock-file packages,
  and installed packages. The supported sections are those listed in
  :doc:`/reference/dune/install` (except ``misc``).

  .. versionadded:: 3.24

The ``%{<kind>:...}`` forms are what allows you to write custom rules that work
transparently, whether things are installed or not.

Note that aliases are ignored by ``%{deps}``. The intent of this last form is to
reliably read a list of strings generated by an OCaml program via:

.. code:: ocaml

    List.iter (fun s -> print_string (String.escaped s)) l

Dealing with circular dependencies introduced by variables
----------------------------------------------------------

If you ever see Dune reporting a dependency cycle that involves a
variable such as ``%{read:<path>}``, try to move ``<path>`` to a different
directory.

The reason you might see such dependency cycle is because Dune is
trying to evaluate the ``%{read:<path>}`` too early. For instance, let's
consider the following example:

.. code:: dune

    (rule
     (targets x)
     (enabled_if %{read:y})
     (action ...))

    (rule
     (with-stdout-to y (...)))

When Dune loads and interprets this file, it decides whether the
first rule is enabled by evaluating ``%{read:y}``. To
evaluate ``%{read:y}``, it must build ``y``. To build ``y``, it must
figure out the build rule that produces ``y``, and in order to do that, it must
first load and evaluate the above ``dune`` file. You can see how this
creates a cycle.

Some cycles might be more complex. In any case, when you see such an
error, the easiest thing to do is move the file that's being read
to a different directory, preferably a standalone one. You can use the
:doc:`/reference/dune/subdir` stanza to keep the logic self-contained in
the same ``dune`` file:

.. code:: dune

    (rule
     (targets x)
     (enabled_if %{read:dir-for-y/y})
     (action ...))

    (subdir
     dir-for-y
     (rule
      (with-stdout-to y (...))))

Expansion of Lists
------------------

Forms that expand to a list of items, such as ``%{cc}``, ``%{deps}``,
``%{targets}``, or ``%{read-lines:...}``, are suitable to be used in
``(run <prog> <arguments>)``. For instance in:

.. code:: dune

    (run foo %{deps})

If there are two dependencies, ``a`` and ``b``, the produced command
will be equivalent to the shell command:

.. code:: console

    $ foo "a" "b"

If you want both dependencies to be passed as a single argument,
you must quote the variable:

.. code:: dune

    (run foo "%{deps}")

which is equivalent to the following shell command:

.. code:: console

    $ foo "a b"

(The items of the list are concatenated with space.)
Please note: since ``%{deps}`` is a list of items, the first one may be
used as a program name. For instance:

.. code:: dune

    (rule
     (targets result.txt)
     (deps    foo.exe (glob_files *.txt))
     (action  (run %{deps})))

Here is another example:

.. code:: dune

    (rule
     (target foo.exe)
     (deps   foo.c)
     (action (run %{cc} -o %{target} %{deps} -lfoolib)))
