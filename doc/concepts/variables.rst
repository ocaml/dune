###########
 Variables
###########

..
   TODO(diataxis)
   - reference: variables
   - explanation: rule loading

Some fields can contains variables that are expanded by Dune. The syntax
of variables is as follows:

.. code::

   %{var}

or, for more complex forms that take an argument:

.. code::

   %{fun:arg}

In order to write a plain ``%{``, you need to write ``\%{`` in a string.

Dune supports the following variables:

-  ``project_root`` is the root of the current project. It is typically
   the root of your project, and as long as you have a ``dune-project``
   file there, ``project_root`` is independent of the workspace
   configuration.

-  ``workspace_root`` is the root of the current workspace. Note that
   the value of ``workspace_root`` isn't constant and depends on whether
   your project is vendored or not.

-  ``cc`` is the C compiler command line (list made of the compiler name
   followed by its flags) that will be used to compile foreign code. For
   more details about its content, please see :ref:`this section
   <flags-flow>`.

-  ``cxx`` is the C++ compiler command line being used in the current
   build context.

-  ``ocaml_bin`` is the path where ``ocamlc`` lives.

-  ``ocaml`` is the ``ocaml`` binary.

-  ``ocamlc`` is the ``ocamlc`` binary.

-  ``ocamlopt`` is the ``ocamlopt`` binary.

-  ``ocaml_version`` is the version of the compiler used in the current
   build context.

-  ``ocaml_where`` is the output of ``ocamlc -where``.

-  ``arch_sixtyfour`` is ``true`` if using a compiler that targets a
   64-bit architecture and ``false`` otherwise.

-  ``null`` is ``/dev/null`` on Unix or ``nul`` on Windows.

-  ``ext_obj``, ``ext_asm``, ``ext_lib``, ``ext_dll``, and ``ext_exe``
   are the file extensions used for various artifacts.

-  ``ext_plugin`` is ``.cmxs`` if ``natdynlink`` is supported and
   ``.cma`` otherwise.

-  ``ocaml-config:v`` is for every variable ``v`` in the output of
   ``ocamlc -config``. Note that Dune processes the output of ``ocamlc
   -config`` in order to make it a bit more stable across versions, so
   the exact set of variables accessible this way might not be exactly
   the same as what you can see in the output of ``ocamlc -config``. In
   particular, variables added in new OCaml versions need to be
   registered in Dune before they can be used.

-  ``profile`` is the profile selected via ``--profile``.

-  ``context_name`` is the name of the context (``default``, or defined
   in the workspace file)

-  ``os_type`` is the type of the OS the build is targeting. This is the
   same as ``ocaml-config:os_type``.

-  ``architecture`` is the type of the architecture the build is
   targeting. This is the same as ``ocaml-config:architecture``.

-  ``model`` is the type of the CPU the build is targeting. This is the
   same as ``ocaml-config:model``.

-  ``system`` is the name of the OS the build is targeting. This is the
   same as ``ocaml-config:system``.

-  ``ignoring_promoted_rule`` is ``true`` if ``--ignore-promoted-rules``
   was passed on the command line and ``false`` otherwise.

-  ``<ext>:<path>`` where ``<ext>`` is one of ``cmo``, ``cmi``, ``cma``,
   ``cmx``, or ``cmxa``. See :ref:`variables-for-artifacts`.

-  ``env:<var>=<default`` expands to the value of the environment
   variable ``<var>``, or ``<default>`` if it does not exist. For
   example, ``%{env:BIN=/usr/bin}``. Available since Dune 1.4.0.

-  There are some Coq-specific variables detailed in
   :ref:`coq-variables`.

In addition, ``(action ...)`` fields support the following special
variables:

-  ``target`` expands to the one target.

-  ``targets`` expands to the list of target.

-  ``deps`` expands to the list of dependencies.

-  ``^`` expands to the list of dependencies, separated by spaces.

-  ``dep:<path>`` expands to ``<path>`` (and adds ``<path>`` as a
   dependency of the action).

-  ``exe:<path>`` is the same as ``<path>``, except when
   cross-compiling, in which case it will expand to ``<path>`` from the
   host build context.

-  ``bin:<program>`` expands ``<path>`` to ``program``. If ``program``
   is installed by a workspace package (see
   :doc:`/reference/files/dune/install` stanzas), the locally built
   binary will be used, otherwise it will be searched in the ``<path>``
   of the current build context. Note that ``(run %{bin:program} ...)``
   and ``(run program ...)`` behave in the same way. ``%{bin:...}`` is
   only necessary when you are using ``(bash ...)`` or ``(system ...)``.

-  ``bin-available:<program>`` expands to ``true`` or ``false``,
   depending on whether ``<program>`` is available or not.

-  ``lib:<public-library-name>:<file>`` expands to the file's
   installation path ``<file>`` in the library
   ``<public-library-name>``. If ``<public-library-name>`` is available
   in the current workspace, the local file will be used, otherwise the
   one from the :term:`installed world` will be used.

-  ``lib-private:<library-name>:<file>`` expands to the file's build
   path ``<file>`` in the library ``<library-name>``. Both public and
   private library names are allowed as long as they refer to libraries
   within the same project.

-  ``libexec:<public-library-name>:<file>`` is the same as ``lib:...``,
   except when cross-compiling, in which case it will expand to the file
   from the host build context.

-  ``libexec-private:<library-name>:<file>`` is the same as
   ``lib-private:...`` except when cross-compiling, in which case it
   will expand to the file from the host build context.

-  ``lib-available:<library-name>`` expands to ``true`` or ``false``
   depending on whether the library is available or not. A library is
   available if at least one of the following conditions holds:

   -  It's part the :term:`installed world`.
   -  It's available locally and is not optional.
   -  It's available locally, and all its library dependencies are
      available.

-  ``version:<package>`` expands to the version of the given package.
   Packages defined in the current scope have priority over the public
   packages. Public packages that don't install any libraries will not
   be detected. How Dune determines the version of a package is
   described :doc:`here <../advanced/package-version>`.

-  ``read:<path>`` expands to the contents of the given file.

-  ``read-lines:<path>`` expands to the list of lines in the given file.

-  ``read-strings:<path>`` expands to the list of lines in the given
   file, unescaped using OCaml lexical convention.

The ``%{<kind>:...}`` forms are what allows you to write custom rules
that work transparently, whether things are installed or not.

Note that aliases are ignored by ``%{deps}``

The intent of this last form is to reliably read a list of strings
generated by an OCaml program via:

.. code:: ocaml

   List.iter (fun s -> print_string (String.escaped s)) l

#. Dealing with circular dependencies introduced by variables

If you ever see Dune reporting a dependency cycle that involves a
variable such as `%{read:<path>}`, try to move `<path>` to a different
directory.

The reason you might see such dependency cycle is because Dune is trying
to evaluate the `%{read:<path>}` too early. For instance, let's consider
the following example:

.. code:: dune

   (rule
    (targets x)
    (enabled_if %{read:y})
    (action ...))

   (rule
    (with-stdout-to y (...)))

When Dune loads and interprets this file, it decides whether the first
rule is enabled by evaluating ``%{read:y}``. To evaluate ``%{read:y}``,
it must build ``y``. To build ``y``, it must figure out the build rule
that produces ``y``, and in order to do that, it must first load and
evaluate the above ``dune`` file. You can see how this creates a cycle.

Some cycles might be more complex. In any case, when you see such an
error, the easiest thing to do is move the file that's being read to a
different directory, preferably a standalone one. You can use the
:doc:`/reference/files/dune/subdir` stanza to keep the logic
self-contained in the same ``dune`` file:

.. code:: dune

   (rule
    (targets x)
    (enabled_if %{read:dir-for-y/y})
    (action ...))

   (subdir
    dir-for-y
    (rule
     (with-stdout-to y (...))))

********************
 Expansion of Lists
********************

Forms that expand to a list of items, such as ``%{cc}``, ``%{deps}``,
``%{targets}``, or ``%{read-lines:...}``, are suitable to be used in
``(run <prog> <arguments>)``. For instance in:

.. code:: dune

   (run foo %{deps})

If there are two dependencies, ``a`` and ``b``, the produced command
will be equivalent to the shell command:

.. code:: console

   $ foo "a" "b"

If you want both dependencies to be passed as a single argument, you
must quote the variable:

.. code:: dune

   (run foo "%{deps}")

which is equivalent to the following shell command:

.. code:: console

   $ foo "a b"

(The items of the list are concatenated with space.) Please note: since
``%{deps}`` is a list of items, the first one may be used as a program
name. For instance:

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
