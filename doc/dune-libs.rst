**************
Dune Libraries
**************

.. TODO(diataxis) Move into :doc:`reference/dune-libs`

.. _configurator:

Configurator
============

Configurator is a small library designed to query features available on the
system in order to generate configuration for Dune builds. Such generated
configuration is usually in the form of command line flags, generated headers,
and stubs, but there are no limitations on this.

Configurator allows you to query for the following features:

* Variables defined in ``ocamlc -config``,

* pkg-config_ flags for packages,

* Test features by compiling C code,

* Extract compile time information such as ``#define`` variables.

Configurator is designed to be cross-compilation friendly and avoids _running_
any compiled code to extract any of the information above.

Configurator started as an `independent library
<https://github.com/janestreet/configurator>`__, but now lives in dune. It is
released as the package ``dune-configurator``.

Usage
-----

We'll describe configurator with a simple example. Everything else can be
easily learned by studying `configurator's API
<https://github.com/ocaml/dune/blob/master/otherlibs/configurator/src/v1.mli>`__.

To use Configurator, write an executable that will query the system using
Configurator's API and output a set of targets reflecting the results. For
example:

.. code-block:: ocaml

  module C = Configurator.V1

  let clock_gettime_code = {|
  #include <time.h>

  int main()
  {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return 0;
  }
  |}

  let () =
    C.main ~name:"foo" (fun c ->
      let has_clock_gettime =
        C.c_test c clock_gettime_code ~link_flags:["-lrt"] in

      C.C_define.gen_header_file c ~fname:"config.h"
        [ "HAS_CLOCK_GETTIME", Switch has_clock_gettime ]);

Usually, the module above would be named ``discover.ml``. The next step is to
invoke it as an executable and tell Dune about the targets that it produces:

.. code-block:: dune

  (executable
   (name discover)
   (libraries dune-configurator))

  (rule
   (targets config.h)
   (action (run ./discover.exe)))

Another common pattern is to produce a flags file with Configurator and then
use this flag file using ``:include``:

.. code-block:: dune

  (library
   (name mylib)
   (foreign_stubs (language c) (names foo))
   (c_library_flags (:include (flags.sexp))))

For this, generate the list of flags for your library (for example, using
``Configurator.V1.Pkg_config``), and then write them to a file: in the above
example, ``flags.sexp`` with ``Configurator.V1.write_flags "flags.sexp"
flags``.

Upgrading From the Old Configurator
-----------------------------------

The old Configurator is the independent `Configurator
<https://github.com/janestreet/configurator>`__ opam package. It's now
deprecated, and users are encouraged to migrate to Dune's own Configurator. The
advantage of the transition include:

* No extra dependencies,

* No need to manually pass ``-ocamlc`` flag,

* New Configurator is cross-compilation compatible.

The following steps must be taken to transition from the old Configurator:

* Mentions of the ``configurator`` opam package should be replaced
  with ``dune-configurator``.

* The library name ``configurator`` should be changed ``dune-configurator``.

* The ``-ocamlc`` flag in rules that runs Configurator scripts should be removed.
  This information is now passed automatically by Dune.

* The new Configurator API is versioned explicitly. The version that's
  compatible with old Configurator is under the ``V1`` module. Hence, to
  transition one's code, it's enough to add this module alias:

.. code-block:: ocaml

   module Configurator = Configurator.V1

.. _pkg-config: https://www.freedesktop.org/wiki/Software/pkg-config/

.. _build-info:

`dune-build-info` Library
=========================

Dune can embed build information such as versions in executables
via the special ``dune-build-info`` library. This library exposes
some information about how the executable was built, such as the
version of the project containing the executable or the list of
statically linked libraries with their versions. Printing the version
at which the current executable was built is as simple as:

.. code:: ocaml

          Printf.printf "version: %s\n"
            (match Build_info.V1.version () with
             | None -> "n/a"
             | Some v -> Build_info.V1.Version.to_string v)

For libraries and executables from development repositories that don't
have version information written directly in the ``dune-project``
file, the version is obtained by querying the version control
system. For instance, the following Git command is used in Git
repositories:

.. code:: console

   $ git describe --always --dirty --abbrev=7

which produces a human readable version string of the form
``<version>-<commits-since-version>-<hash>[-dirty]``.

Note that in the case where the version string is obtained from the version
control system, the version string will only be written in the binary once it's
installed or promoted to the source tree. In particular, if you evaluate this
expression as part of your package build, it will return ``None``. This ensures
that committing doesn't hurt your development experience. Indeed, if Dune
stored the version directly inside the freshly built binaries, then every time
you commit your code, the version would change and Dune would need to rebuild
all the binaries and everything that depends on them, such as tests. Instead,
Dune leaves a placeholder inside the binary and fills it during installation or
promotion.

.. _dune-action-plugin:

(Experimental) Dune Action Plugin
=================================

*This library is experimental and no backwards compatibility is implied. Use at
your own risk.*

``Dune-action-plugin`` provides a monadic interface to express program
dependencies directly inside the source code. Programs using this feature
should be declared using :doc:`/reference/actions/dynamic-run` instead of usual
:doc:`/reference/actions/run`.
