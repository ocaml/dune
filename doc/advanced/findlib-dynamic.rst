Dynamic Loading of Packages with Findlib
========================================

.. TODO(diataxis) this is an howto

The preferred way for new development is to use :ref:`plugins`.

Dune supports the ``findlib.dynload`` package from `Findlib
<http://projects.camlcity.org/projects/findlib.html>`_ that enables
dynamically-loading packages and their dependencies (using the OCaml Dynlink module).
Adding the ability for an application to have plugins just requires adding
``findlib.dynload`` to the set of library dependencies:

.. code:: dune

    (library
      (name mytool)
      (public_name mytool)
      (modules ...)
    )

    (executable
      (name main)
      (public_name mytool)
      (libraries mytool findlib.dynload)
      (modules ...)
    )


Use ``Fl_dynload.load_packages l`` in your application to load 
the list ``l`` of packages. The packages are loaded
only once, so trying to load a package statically linked does nothing.

A plugin creator just needs to link to your library:

.. code:: dune

    (library
      (name mytool_plugin_a)
      (public_name mytool-plugin-a)
      (libraries mytool)
    )

For clarity, choose a naming convention. For example, all the plugins of
``mytool`` should start with ``mytool-plugin-``. You can automatically
load all the plugins installed for your tool by listing the existing packages:

.. code:: ocaml

    let () = Findlib.init ()
    let () =
      let pkgs = Fl_package_base.list_packages () in
      let pkgs =
        List.filter
          (fun pkg -> 14 <= String.length pkg && String.sub pkg 0 14 = "mytool-plugin-")
          pkgs
      in
      Fl_dynload.load_packages pkgs
