.. _sites:

***************************************
How to load additional files at runtime
***************************************

There are many ways for applications to load files at runtime and Dune provides
a well tested, key-in-hand portable system for doing so. The Dune model works by
defining ``sites`` where files will be installed and looked up at runtime. At
runtime, each site is associated to a list of directories which contain the
files added in the site.

*WARNING*: This feature remains experimental and is subject to breaking changes
without warning. It must be explicitly enabled in the ``dune-project`` file with
``(using dune_site 0.1)``

Sites
=====

Defining a site
---------------

A site is defined in a package :ref:`package` in the ``dune-project`` file. It
consists of a name and a :ref:`section<install>` (e.g ``lib``, ``share``,
``etc``) where the site will be installed as a sub-directory.

.. code:: scheme

   (lang dune 2.8)
   (using dune_site 0.1)
   (name mygui)

   (package
    (name mygui)
    (sites (share themes)))

Adding files to a site
----------------------

Here the package ``mygui`` defines a site named ``themes`` that will be located
in the section ``share``. This package can add files to this ``sites`` using the
:ref:`install stanza<install>`:

.. code:: scheme

   (install
    (section (site mygui themes))
    (files
     (layout.css as default/layout.css)
     (ok.png  as default/ok.png)
     (ko.png  as default/ko.png)))

Another package ``mygui_material_theme`` can install files inside ``mygui``
directory for adding a new theme. Inside the scope of ``mygui_material_theme``
the ``dune`` file contains:

.. code:: scheme

   (install
    (section (site mygui themes))
    (files
     (layout.css as material/layout.css)
     (ok.png  as material/ok.png)
     (ko.png  as material/ko.png)))

The package ``mygui`` must be present in the workspace or installed.

.. warning::

   Two files should not be installed by different packages at the same destination.

Getting the locations of a site at runtime
------------------------------------------

The executable ``mygui`` will be able to get the locations of the ``themes``
site using the :ref:`generate sites module stanza<generate_sites_module>`

.. code:: scheme

   (executable
    (name mygui)
    (modules mygui mysites)
    (libraries dune-site))

   (generate_sites_module
    (name mysites)
    (sites mygui))

The generated module `mysites` depends on the library `dune-site` provided by
Dune.

Then inside ``mygui.ml`` module the locations can be recovered and used:

.. code:: ocaml

   (** Locations of the site for the themes *)
   let themes_locations : string list = Mysites.Sites.themes

   (** Merge the content of the directories in [dirs] *)
   let rec readdirs dirs =
     List.concat
       (List.map
          (fun dir -> Array.to_list (Sys.readdir dir))
          (List.filter Sys.file_exists dirs))

   (** Get the lists of the available themes  *)
   let find_available_themes () : string list = lookup_dirs themes_locations

   (** Lookup a file in the directories *)
   let rec lookup_file filename = function
     | [] -> raise Not_found
     | dir::dirs ->
        let filename' = Filename.concat dir filename in
        if Sys.file_exists filename' then filename'
        else lookup_file filename dirs

   (** [lookup_theme_file theme file] get the [file] of the [theme] *)
   let lookup_theme_file file theme =
     lookup_file (Filename.concat theme file) themes_locations

   let get_layout_css = lookup_theme_file "layout.css"
   let get_ok_ico = lookup_theme_file "ok.png"
   let get_ko_ico = lookup_theme_file "ko.png"


Tests
-----

During tests the files are copied into the sites through the dependency
``(package mygui)`` and ``(package mygui_material_theme)`` as for other files in
install stanza.

Installation
------------

Installation is done simply with ``dune install``, however if one want to
install this tool such that it is relocatable, one can use ``dune
install --relocatable --prefix $dir``. The files will be copied to the directory
``$dir`` but the binary ``$dir/bin/mygui`` will find the site location relative
to its location. So even if the directory ``$dir`` is moved,
``themes_locations`` will be correct.

Implementation details
----------------------

The main difficulty for sites is that their directories are found at different
locations at different times:

- When the package is available locally, the location is inside ``_build``
- When the package is installed, the location is inside the install prefix
- If a local package wants to install files to the site of another installed
  package the location is at the same time in ``_build`` and in the install prefix
  of the second package.

With the last example we see that the location of a site is not always a single
directory, but can consist of a sequence of directories: ``["dir1" ; "dir2"]``.
So a lookup must first look into `dir1`, then into `dir2`.

.. _plugins:

Plugins and dynamic loading of packages
========================================

Dune allows to define and load plugins without having to deal with specific
compilation, installation directories, dependencies, or the Dynlink_ module.

To define a plugin:

- The package defining the plugin interface must define a `site` where the
  plugins must live. Traditionally, this is in ``(lib plugins)``, but it is just
  a convention.

- Define a library that each plugin must use to register itself (or otherwise
  provide its functionality).

- Define the plugin in another package using the `plugin` stanza.

- Generate a module that may load all available plugins using the
  `generated_module` stanza.

Example
-------

We demonstrate an example of the scheme above. The example consists of the
following components:

Inside package `c`,

- A package `c`, containing the executable `c`, that we intend to extend with
  plugins.

- A library `c.register` which defines the plugin registration interface.

- A generated module `Sites` which can load available plugins at runtime.

- An executable `c` that will use the module `Sites` to load all the plugins.

Inside package `b`, we declare plugin using the `c.register` api and the
`plugin` stanza.

Main executable (C)
^^^^^^^^^^^^^^^^^^^^^

- ``dune-project`` file:

.. code:: scheme

   (lang dune 2.8)
   (using dune_site 0.1)
   (name c)
   (package
    (name c)
    (sites (lib plugins)))


- ``dune`` file:

.. code:: scheme

   (executable
    (public_name c)
    (modules sites c)
    (libraries c.register dune-site dune-site.plugins))

   (library
    (public_name c.register)
    (name c_register)
    (modules c_register))

   (generate_sites_module
    (module sites)
    (plugins (c plugins)))

The generated module `sites` depends here also on the library
`dune-site.plugins` because the plugins optional field is requested.

- The module ``c_register.ml`` of the library ``c.register``:

.. code:: ocaml

   let todo = Queue.create ()

- The code of the executable ``c.ml``:

.. code:: ocaml

   (* load all the available plugins *)
   let () = Sites.Plugins.Plugins.load_all ()
   (* Execute the code registered by the plugins *)
   let () = Queue.iter (fun f -> f ()) !C_register.todo

One plugin (B)
^^^^^^^^^^^^^^

- ``dune-project`` file:

.. code:: scheme

   (lang dune 2.8)
   (using dune_site 0.1)
   (name b)

- ``dune`` file:

.. code:: scheme

  (library
   (public_name b)
   (libraries c.register))

  (plugin
   (name b)
   (libraries b)
   (site (c plugins)))

- The code of the plugin ``b.ml``:

.. code:: ocaml

   let () =
     Queue.add (fun () -> print_endline "B is doing something") C_register.todo

.. _Dynlink: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Dynlink.html
