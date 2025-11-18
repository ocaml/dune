How to make a Homebrew Package with Dune
========================================

This guide will show you how to make a Homebrew package for an application
using Dune package management. The only dependency of the Homebrew package will
be Dune, and all OCaml dependencies (including the OCaml compiler) will be
installed by Dune while building the Homebrew package.

To use Dune package management to build a project as a Homebrew package, the
project must have a source archive hosted online somewhere (e.g. a gzipped
tarball on the project's Github release page).

Before making a Homebrew package, it's a good idea to familiarize yourself with
Homebrew's terminology and packaging conventions `here
<https://docs.brew.sh/Adding-Software-to-Homebrew>`__.

Homebrew packages are recommended to be source-based, and for the source code
to be explicitly versioned, so for this example assume ``my_app`` has a
versioned archive hosted on Github with version ``0.1.0``.

Homebrew can generate a starting point for a formula if you point it at a
source archive hosted on Github:

.. code:: console

  $ brew create https://github.com/me/my_app/archive/refs/tags/0.1.0.tar.gz

A source archive like the one in the above command is generated when you
release a project on Github. The above command will generate a file named
``my_app.rb`` in your current tap. All the project metadata will be filled in
automatically based on the project on Github. All we need to do now is to
specify dependencies and the commands ``brew`` should run when installing the
package.

Here's the complete formula for ``my_app``. Note that the ``test`` section is
intended to be a sanity check of the core functionality of the package, not a
complete integration test suite. Read more about Homebrew package tests `here
<https://docs.brew.sh/Formula-Cookbook#add-a-test-to-the-formula>`__. Note
however that tests run in an environment without access to build dependencies
such as Dune, so ``dune runtest`` can't be used to test Homebrew packages.

.. code:: ruby

  class MyApp < Formula
    desc "My awesome app"
    homepage "https://github.com/me/my_app"
    url "https://github.com/me/my_app/releases/download/0.1.0/0.1.0.tar.gz"
    sha256 "eb8705de406441675747a639351d0d59bffe7b9f5b05ec9b6e11b4c4c9d7a6ee"
    license "MIT"

    depends_on "dune" => :build

    def install
      # Uncomment if the source archive lacks a lockdir:
      # system "dune", "pkg", "lock"
      system "dune", "build", "@install", "--release", "--only-packages", "my_app"
      system "dune", "install", "--prefix=#{prefix}", "my_app"
    end

    test do
      # Test your application here!
      system "my_app", "--version"
    end
  end

This assumes that the name of the package in the source archive is ``my_app``.
That is, the archive contains a ``dune-project`` file defining a package named
``my_app``, or that the archive contains a ``my_app.opam``. The archive may
contain multiple packages provided that ``my_app`` is one of them.

Note the comment at the beginning of the ``install`` method. The commented-out
code invokes Dune's solver to compute the transitive closure of packages that
will be built as dependencies of ``my_app``, and stores the result in a
directory named ``dune.lock`` - a "Lock Directory" or "lockdir" for short.
Solving dependencies in the ``install`` method should be avoided when possible.
This is because Dune solves dependencies in the context of the current tip of
the `Opam Repository <https://github.com/ocaml/opam-repository>`_, which
changes as new Opam packages are released. This means that the exact solution
computed by Dune can change as new versions of dependencies come out. Homebrew
encourages package builds to be `reproducible
<https://docs.brew.sh/Reproducible-Builds>`_ when possible, but solving
dependencies each time a package is installed prevents that package from being
built reproducibly. To allow reproducible builds, always include the project's
lockdir in its source archive (generate a lockdir by running ``dune pkg lock``)
when releasing a package. Only solve dependencies in the ``install`` method
when packaging a project whose source archive lacks a lockdir.

If there are any packages with external dependencies (i.e. ``depexts``) in
``my_app``'s transitive dependency closure, their corresponding Homebrew
package must be added as a dependency of ``my_app``'s Homebrew packages by
adding a ``depends_on`` entry for each. List all the external dependencies
among ``my_app``'s transitive dependency closure by running:

.. code:: console

   $ dune show depexts

External dependencies can be platform-specific, so if you're planning to make the
Homebrew package available for macOS, be sure to run the above command on a Mac
to determine which external dependencies need to be added to the formula.
