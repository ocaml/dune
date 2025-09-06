library_parameter
-----------------

.. warning::

   This feature is experimental and requires the compiler you are using to
   support parameterized libraries.

The ``library_parameter`` stanza describes a parameter interface defined in a single ``.mli`` file. To enable this feature,
you need to add ``(using oxcaml 0.1)`` :doc:`extension
</reference/dune-project/using>` in your ``dune-project`` file.

.. confval:: (library_parameter ...)

  .. versionadded:: 3.20

  Define a parameter.

  .. confval:: (name <parameter-name>)

    ``parameter-name`` is the name of the library parameter. It must be a valid
    OCaml module name as for :doc:`/reference/dune/library`. 

    This must be specified if no `public_name` is specified.

  .. confval:: (public_name ...)

    The name under which the library parameter can be referred as a dependency
    when it's not part of the current workspace, i.e., when it is installed.
    Without a ``(public_name ...)`` field, the library parameter won't be
    installed by Dune. The public name must start with the package name it's
    part of and optionally followed by a dot, then anything else you want. The
    package name must also be one of the packages that Dune knows about, as
    determined by the logic described in :doc:`/reference/packages`.

  .. confval:: (package <package>)

    Installs a private library parameter under the specified package. Such a
    parameter is now usable by public libraries defined in the same project.

  .. confval:: (synopsis <string>)

    A one-line description of the library parameter.

  .. confval:: (modules <modules>)

    Specifies a specific module to select as a `library_parameter`.

    ``<modules>`` uses the :doc:`/reference/ordered-set-language`, where
    elements are module names and don't need to start with an uppercase letter.

    The library parameter **must** only declare one ``mli`` file as part of its
    modules.

  .. confval:: (libraries <library-dependencies>)

    Specifies the library parameter's dependencies.

    See :doc:`/reference/library-dependencies` for more details.

  .. confval:: (preprocesss <preprocess-spec>)

    Specifies how to preprocess files when needed.

    The default is ``no_preprocessing``, and other options are described
    in :doc:`/reference/preprocessing-spec`.

  .. confval:: (preprocessor_deps (<deps-conf list>))

    Specifies extra preprocessor dependencies preprocessor, i.e., if the
    preprocessor reads a generated file.

    The specification of dependencies is described in
    :doc:`/concepts/dependency-spec`.


  .. confval:: (flags ...)

    See :doc:`/concepts/ocaml-flags`.

  .. confval:: (ocamlc_flags ...)

    See :doc:`/concepts/ocaml-flags`.

  .. confval:: (ocamlopt_flags ...)

   See :doc:`/concepts/ocaml-flags`.

  .. confval:: (optional)

    If present, it indicates that the library parameter should only be built
    and installed if all the dependencies are available, either in the
    workspace or in the installed world.

  .. confval:: (enabled_if <blang expression>)

    Conditionally disables a library parameter.

    A disabled library parameter cannot be built and will not be installed.

    The condition is specified using the :doc:`/reference/boolean-language`, and
    the field allows for the ``%{os_type}`` variable, which is expanded to the
    type of OS being targeted by the current build. Its value is the same as the
    value of the ``os_type`` parameter in the output of ``ocamlc -config``.

  .. confval:: (allow_overlapping_dependencies)

    Allows external dependencies to overlap with libraries that are present in
    the workspace.
