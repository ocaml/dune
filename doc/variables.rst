.. _variables:

*******************
Variables expansion
*******************

Some fields can contains variables of the form ``%{var}`` that are
expanded by dune.

Dune supports the following variables:

- ``project_root`` is the root of the current project. It is typically the root
  of your project and as long as you have a ``dune-project`` file there,
  ``project_root`` is independent of the workspace configuration
- ``workspace_root`` is the root of the current workspace. Note that
  the value of ``workspace_root`` is not constant and depends on
  whether your project is vendored or not
-  ``CC`` is the C compiler command line (list made of the compiler
   name followed by its flags) that was used to compile OCaml in the
   current build context
-  ``CXX`` is the C++ compiler command line being used in the
   current build context
-  ``ocaml_bin`` is the path where ``ocamlc`` lives
-  ``ocaml`` is the ``ocaml`` binary
-  ``ocamlc`` is the ``ocamlc`` binary
-  ``ocamlopt`` is the ``ocamlopt`` binary
-  ``ocaml_version`` is the version of the compiler used in the
   current build context
-  ``ocaml_where`` is the output of ``ocamlc -where``
-  ``arch_sixtyfour`` is ``true`` if using a compiler targeting a
   64 bit architecture and ``false`` otherwise
-  ``null`` is ``/dev/null`` on Unix or ``nul`` on Windows
-  ``ext_obj``, ``ext_asm``, ``ext_lib``, ``ext_dll`` and ``ext_exe``
   are the file extension used for various artifacts
- ``ocaml-config:v`` for every variable ``v`` in the output of
  ``ocamlc -config``. Note that dune processes the output
  of ``ocamlc -config`` in order to make it a bit more stable across
  versions, so the exact set of variables accessible this way might
  not be exactly the same as what you can see in the output of
  ``ocamlc -config``. In particular, variables added in new versions
  of OCaml needs to be registered in dune before they can be used
- ``profile`` the profile selected via ``--profile``
- ``context_name`` the name of the context (``default`` or defined in the
  workspace file)
- ``os_type`` is the type of the OS the build is targetting. This is
  the same as ``ocaml-config:os_type``
- ``architecture`` is the type of the architecture the build is targetting. This
  is the same as ``ocaml-config:architecture``
- ``model`` is the type of the cpu the build is targetting. This is
  the same as ``ocaml-config:model``
- ``system`` is the name of the OS the build is targetting. This is the same as
  ``ocaml-config:system``
- ``ignoring_promoted_rule`` is ``true`` if
  ``--ignore-promoted-rules`` was passed on the command line and
  ``false`` otherwise

In addition, ``(action ...)`` fields support the following special variables:

- ``target`` expands to the one target
- ``targets`` expands to the list of target
- ``deps`` expands to the list of dependencies
- ``^`` expands to the list of dependencies, separated by spaces
- ``dep:<path>`` expands to ``<path>`` (and adds ``<path>`` as a dependency of
  the action)
- ``exe:<path>`` is the same as ``<path>``, except when cross-compiling, in
  which case it will expand to ``<path>`` from the host build context
- ``bin:<program>`` expands to a path to ``program``. If ``program``
  is installed by a package in the workspace (see :ref:`install` stanzas),
  the locally built binary will be used, otherwise it will be searched
  in the ``PATH`` of the current build context. Note that ``(run
  %{bin:program} ...)`` and ``(run program ...)`` behave in the same
  way. ``%{bin:...}`` is only necessary when you are using ``(bash
  ...)`` or ``(system ...)``
- ``lib:<public-library-name>:<file>`` expands to a path to file ``<file>`` of
  library ``<public-library-name>``. If ``<public-library-name>`` is available
  in the current workspace, the local file will be used, otherwise the one from
  the installed world will be used
- ``libexec:<public-library-name>:<file>`` is the same as ``lib:...`` except
  when cross-compiling, in which case it will expand to the file from the host
  build context
- ``lib-available:<library-name>`` expands to ``true`` or ``false`` depending on
  whether the library is available or not. A library is available iff at least
  one of the following condition holds:

  -  it is part the installed worlds
  -  it is available locally and is not optional
  -  it is available locally and all its library dependencies are
     available

- ``version:<package>`` expands to the version of the given
  package. Note that this is only supported for packages that are
  being defined in the current scope
- ``read:<path>`` expands to the contents of the given file
- ``read-lines:<path>`` expands to the list of lines in the given
  file
- ``read-strings:<path>`` expands to the list of lines in the given
  file, unescaped using OCaml lexical convention

The ``%{<kind>:...}`` forms are what allows you to write custom rules that work
transparently whether things are installed or not.

Note that aliases are ignored by ``%{deps}``

The intent of this last form is to reliably read a list of strings
generated by an OCaml program via:

.. code:: ocaml

    List.iter (fun s -> print_string (String.escaped s)) l

#. Expansion of lists

Forms that expands to list of items, such as ``%{cc}``, ``%{deps}``,
``%{targets}`` or ``%{read-lines:...}``, are suitable to be used in, say,
``(run <prog> <arguments>)``.  For instance in:

.. code:: scheme

    (run foo %{deps})

if there are two dependencies ``a`` and ``b``, the produced command
will be equivalent to the shell command:

.. code:: shell

    $ foo "a" "b"

If you want the two dependencies to be passed as a single argument,
you have to quote the variable as in:

.. code:: scheme

    (run foo "%{deps}")

which is equivalent to the following shell command:

.. code:: shell

    $ foo "a b"

(the items of the list are concatenated with space).
Note that, since ``%{deps}`` is a list of items, the first one may be
used as a program name, for instance:

.. code:: scheme

    (rule
     (targets result.txt)
     (deps    foo.exe (glob_files *.txt))
     (action  (run %{deps})))

Here is another example:

.. code:: scheme

    (rule
     (target foo.exe)
     (deps   foo.c)
     (action (run %{cc} -o %{target} %{deps} -lfoolib)))
