***********************************
Versions of OCaml supported by Dune
***********************************

This part of the manual explains the policy used by dune to decide
what versions of the OCaml compiler it supports. By supporting version
X of the OCaml compiler, we mean the following:

- dune can be built using version X of the OCaml compiler
- dune can build projects using version X of the OCaml compiler

General policy
==============

Dune supports the latest minor releases of the last 4 major releases
of the compiler. For instance, if the latest release of the compiler
is 4.07.0, then dune may supports the following versions:

- 4.04.2
- 4.05.0
- 4.06.1
- 4.07.0

Minor releases in between such as 4.06.0 may or may not be supported
by dune. They are in general supported, however some of them may have
serious bugs that makes it impossible for dune to provide good support
for them.

This policy will be enforced starting from January 2019. Before that,
dune supports all OCaml versions since 4.02.3.

Supporting older versions of the compiler
=========================================

It is possible to write a project using dune that will support more
than 4 OCaml versions. To do that, simply make sure to limit the
version of the dune language you are using to one that is supported by
an older version of dune supporting this version of the compiler.

For instance, if the last version of dune supporting OCaml 4.02.3 was
1.5.0 and you want to support both OCaml 4.02.3 and OCaml 4.08.0,
simply use a version of the dune language that is less or equal to
1.5:

.. code:: scheme

          (lang dune 1.5)

Versions of dune starting from 1.5.0 onwards will support this
language for a reasonable amount of time.  There is currently no
policy on how long a given version of the dune language will be
supported as we don't yet have enough experience with how much works
it represent, but it is expected to be long enough.

Maintenance of older versions of dune
=====================================

Whenever dune drops support for one major version of the compiler, the
dune team will still support the previous version of dune until the
next version drop. For instance if dune 1.5.0 is the last version of
dune to support OCaml 4.05.X and dune 1.8.0 is the last version of
dune to support OCaml 4.06.X, then dune 1.5.X will be maintained until
dune 1.9.0 is released. By support we mean the following:

- bugs will be fixed
- bug fix releases of the 1.5.X branch will be made in the opam
  repository
- no new features will be added or accepted in the 1.5.X branch

After the next version drop, support will no longer be provided and it
will be up to the community of dune users to maintain older versions
of dune if they wish to do so.
