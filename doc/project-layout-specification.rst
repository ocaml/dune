*****************************************
Project Layout and Metadata Specification
*****************************************

A typical jbuilder project will have one or more ``<package>.opam`` file
at toplevel as well as ``jbuild`` files wherever interesting things are:
libraries, executables, tests, documents to install, etc...

It is recommended to organize your project so that you have exactly one
library per directory. You can have several executables in the same
directory, as long as they share the same build configuration. If you'd
like to have multiple executables with different configurations in the
same directory, you will have to make an explicit module list for every
executable using ``modules``.

The next sections describe the format of Jbuilder metadata files.

Note that the Jbuilder metadata format is versioned in order to ensure
forward compatibility. There is currently only one version available,
but to be future proof, you should still specify it in your ``jbuild``
files. If no version is specified, the latest one will be used.

.. _metadata-format:

Metadata format
===============

Most configuration files read by Jbuilder are using the S-expression
syntax, which is very simple.  It is described below.

Note that the format is completely static. However you can do
meta-programming on jbuilds files by writing them in :ref:`ocaml-syntax`.


Lexical conventions of s-expressions
------------------------------------

Whitespace, which consists of space, newline, horizontal tab, and form
feed, is ignored unless within an OCaml-string, where it is treated
according to OCaml-conventions.  The left parenthesis opens a new
list, the right one closes it.  Lists can be empty.

The double quote denotes the beginning and end of a string using
similar lexing conventions to the ones of OCaml (see the OCaml-manual
for details).  Differences are:

- octal escape sequences (``\o123``) are not supported;
- backslash that's not a part of any escape sequence is kept as it is
  instead of resulting in parse error;
- a backslash followed by a space does not form an escape sequence, so
  it’s interpreted as is, while it is interpreted as just a space by
  OCaml.

All characters other than double quotes, left- and right parentheses,
whitespace, carriage return, and comment-introducing characters or
sequences (see next paragraph) are considered part of a contiguous
string.

Comments
--------

There are three kinds of comments:

- line comments are introduced with ``;``, and end at the newline;
- sexp comments are introduced with ``#;``, and end at the end of the
  following s-expression;
- block comments are introduced with ``#|`` and end with ``|#``.
  These can be nested, and double-quotes within them must be balanced
  and be lexically correct OCaml strings.

Grammar of s-expressions
------------------------

S-expressions are either sequences of non-whitespace characters
(= atoms), doubly quoted strings or lists. The lists can recursively
contain further s-expressions or be empty, and must be balanced,
i.e. parentheses must match.

Examples
--------

::

    this_is_an_atom_123'&^%!  ; this is a comment
    "another atom in an OCaml-string \"string in a string\" \123"
    
    ; empty list follows below
    ()
    
    ; a more complex example
    (
      (
        list in a list  ; comment within a list
        (list in a list in a list)
        42 is the answer to all questions
        #; (this S-expression
             (has been commented out)
           )
        #| Block comments #| can be "nested" |# |#
      )
    )


.. _opam-files:

<package>.opam files
====================

When a ``<package>.opam`` file is present, Jbuilder will know that the
package named ``<package>`` exists. It will know how to construct a
``<package>.install`` file in the same directory to handle installation
via `opam <https://opam.ocaml.org/>`__. Jbuilder also defines the
recursive ``install`` alias, which depends on all the buildable
``<package>.install`` files in the workspace. So for instance to build
everything that is installable in a workspace, run at the root:

::

    $ jbuilder build @install

Declaring a package this way will allow you to add elements such as
libraries, executables, documentation, ... to your package by declaring
them in ``jbuild`` files.

Such elements can only be declared in the scope defined by the
corresponding ``<package>.opam`` file. Typically, your
``<package>.opam`` files should be at the root of your project, since
this is where ``opam pin ...`` will look for them.

Note that ``<package>`` must be non-empty, so in particular ``.opam``
files are ignored.

.. _scopes:

Scopes
------

Any directory containing at least one ``<package>.opam`` file defines
a scope. This scope is the sub-tree starting from this directory,
excluding any other scopes rooted in sub-direcotries.

Typically, any given project will define a single scope. Libraries and
executables that are not meant to be installed will be visible inside
this scope only.

Because scopes are exclusive, if you whish to include the dependencies
of the project you are currently working on into your workspace, you
may copy them in a ``vendor`` directory, or any other name of your
choice. Jbuilder will look for them there rather than in the installed
world and there will be no overlap between the various scopes.

Package version
---------------

Note that Jbuilder will try to determine the version number of packages
defined in the workspace. While Jbuilder itself makes no use of version
numbers, it can be use by external tools such as
`ocamlfind <http://projects.camlcity.org/projects/findlib.html>`__.

Jbuilder determines the version of a package by first looking in the
``<package>.opam`` for a ``version`` variable. If not found, it will try
to read the first line of a version file in the same directory as the
``<package>.opam`` file. The version file is any file whose name is, in
order in which they are looked for:

-  ``<package>.version``
-  ``version``
-  ``VERSION``

The version file can be generated by a user rule.

If the version can't be determined, Jbuilder just won't assign one.

Note that if you are using `Topkg <https://github.com/dbuenzli/topkg>`__
as well in your project, you shouldn't manually set a version in your
``<package>.opam`` file or write/generate on of the file listed above.
See the section about :ref:`using-topkg` for more details.

Odig conventions
----------------

Jbuilder follows the `odig <http://erratique.ch/software/odig>`__
conventions and automatically installs any README\*, CHANGE\*, HISTORY\*
and LICENSE\* files in the same directory as the ``<package>.opam`` file
to a location where odig will find them.

Note that this includes files present in the source tree as well as
generated files. So for instance a changelog generated by a user rule
will be automatically installed as well.

jbuild-ignore
=============

By default Jbuilder traverses the whole source tree, ignoring the
following files and directories:

- any file that start with ``.#``
- any directory that start with either ``.`` or ``_``

To ignore a subtree, simply write a ``jbuild-ignore`` file in the
parent directory containing the name of the sub-directories to ignore.

So for instance, if you write ``foo`` in ``src/jbuild-ignore``, then
``src/foo`` won't be traversed and any ``jbuild`` file it contains will
be ignored.

``jbuild-ignore`` files contain a list of directory names, one per line.
