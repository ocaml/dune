generate_sites_module
---------------------

.. versionadded:: 2.8

Dune proposes some facilities for dealing with :ref:`sites<sites>` in a program.
The ``generate_sites_module`` stanza will generate code for looking up the
correct locations of the sites' directories and for loading plugins. It works
after installation with or without the relocation mode, inside Dune rules, and
when using Dune executables. For promotion, it works only if the generated
modules are solely in the executable (or library statically linked) promoted;
generated modules in plugins won't work.

.. code:: dune

   (generate_sites_module
    (module <name>)
    <facilities>)

The module's code is generated in the directory with the given name. The code is
populated according to the requested facilities.


The available ``<facilities>`` are:

- ``sourceroot`` adds a value ``val sourceroot: string option`` in the generated
  module, which contains the value of ``%{workspace_root}``, if the code has
  been built locally. It could be used to keep the tool's configuration file
  locally when executed with ``dune exec`` or after promotion. The value is
  ``None`` once it has been installed.

- ``relocatable`` adds a value ``val relocatable: bool`` in the generated
  module, which indicates if the binary has been installed in the relocatable
  mode.

- ``(sites <package>)`` adds a value ``val <site>: string list`` for each
  ``<site>`` of ``<package>`` in the submodule `Sites` of the generated module.
  The identifier <site> isn't capitalized.

- ``(plugins (<package> <site>) ...)`` adds a submodule ``<site>`` with the
  following signature ``S`` in the submodule ``Plugins`` of the generated module
  . The identifier ``<site>`` is capitalized.

.. code:: ocaml

   module type S = sig
     val paths: string list
     (** return the locations of the directory containing the plugins *)

     val list: unit -> string list
     (** return the list of available plugins *)

     val load_all: unit -> unit
     (** load all the plugins and their dependencies *)

     val load: string -> unit
     (** load the specified plugin and its dependencies *)
   end

The generated module is a dependency on the library ``dune-site``, and if the
facilities ``(plugins ...)`` are used, it is a dependency on the library
``dune-site.plugins``. Those dependencies are not automatically added to the
library or executable which use the module (cf. :ref:`plugins`).
