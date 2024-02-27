##########################
 Dependency Specification
##########################

..
   TODO(diataxis)
   - reference - dependency spec
   - reference - globbing

Dependencies in ``dune`` files can be specified using one of the
following:

.. _source_tree:

-  ``(:name <dependencies>)`` will bind the list of dependencies to the
   ``name`` variable. This variable will be available as ``%{name}`` in
   actions.

-  ``(file <filename>)``, or simply ``<filename>``, depend on this file.

-  ``(alias <alias-name>)`` depends on the construction of this alias.
   For instance: ``(alias src/runtest)``.

-  ``(alias_rec <alias-name>)`` depends on the construction of this
   alias recursively in all children directories wherever it is defined.
   For instance: ``(alias_rec src/runtest)`` might depend on ``(alias
   src/runtest)``, ``(alias src/foo/bar/runtest)``, etc.

-  ``(glob_files <glob>)`` depends on all files matched by ``<glob>``.
   See the :ref:`glob <glob>` for details.

-  ``(glob_files_rec <glob>)`` is the recursive version of ``(glob_files
   <glob>)``. See the :ref:`glob <glob>` for details.

-  ``(source_tree <dir>)`` depends on all source files in the subtree
   with root ``<dir>``.

-  ``(universe)`` depends on everything in the universe. This is for
   cases where dependencies are too hard to specify. Note that Dune will
   not be able to cache the result of actions that depend on the
   universe. In any case, this is only for dependencies in the
   :term:`installed world`. You must still specify all dependencies that
   come from the workspace.

-  ``(package <pkg>)`` depends on all files installed by ``<package>``,
   as well as on the transitive package dependencies of ``<package>``.
   This can be used to test a command against the files that will be
   installed.

-  ``(env_var <var>)`` depends on the value of the environment variable
   ``<var>``. If this variable becomes set, becomes unset, or changes
   value, the target will be rebuilt.

-  ``(sandbox <config>)`` requires a particular sandboxing
   configuration. ``<config>`` can be one (or many) of:

   -  ``always``: the action requires a clean environment
   -  ``none``: the action must run in the build directory
   -  ``preserve_file_kind``: the action needs the files it reads to
      look like normal files (so Dune won't use symlinks for sandboxing)

-  ``(include <file>)`` read the s-expression in ``<file>`` and
   interpret it as additional dependencies. The s-expression is expected
   to be a list of the same constructs enumerated here.

In all these cases, the argument supports :doc:`variables`.

********************
 Named Dependencies
********************

Dune allows a user to organize dependency lists by naming them. The user
is allowed to assign a group of dependencies a name that can later be
referred to in actions (like the ``%{deps}``, ``%{target}``, and
``%{targets}`` built in variables).

One instance where this is useful is for naming globs. Here's an example
of an imaginary bundle command:

.. code:: dune

   (rule
    (target archive.tar)
    (deps
     index.html
     (:css (glob_files *.css))
     (:js foo.js bar.js)
     (:img (glob_files *.png) (glob_files *.jpg)))
    (action
     (run %{bin:bundle} index.html -css %{css} -js %{js} -img %{img} -o %{target})))

Note that a named dependency list can also include unnamed dependencies
(like ``index.html`` in the example above). Also, such user defined
names will shadow build in variables, so ``(:workspace_root x)`` will
shadow the built-in ``%{workspace_root}`` variable.

.. _glob:

******
 Glob
******

You can use globs to declare dependencies on a set of files. Note that
globs will match files that exist in the source tree as well as
buildable targets, so for instance you can depend on ``*.cmi``.

Dune supports globbing files in a single directory via ``(glob_files
...)`` and, starting with Dune 3.0, in all subdirectories recursively
via ``(glob_files_rec ...)``. The glob is interpreted as follows:

-  anything before the last ``/`` is taken as a literal path
-  anything after the last ``/``, or everything if the glob contains no
   ``/``, is interpreted using the glob syntax

Absolute paths are permitted in the ``(glob_files ...)`` term only. It's
an error to pass an absolute path (i.e., a path beginning with a ``/``)
to ``(glob_files_rec ...)```.

The glob syntax is interpreted as follows:

-  ``\<char>`` matches exactly ``<char>``, even if it's a special
   character (``*``, ``?``, ...).

-  ``*`` matches any sequence of characters, except if it comes first,
   in which case it matches any character that is not ``.`` followed by
   anything.

-  ``**`` matches any character that is not ``.`` followed by anything,
   except if it comes first, in which case it matches anything.

-  ``?`` matches any single character.

-  ``[<set>]`` matches any character that is part of ``<set>``.

-  ``[!<set>]`` matches any character that is not part of ``<set>``.

-  ``{<glob1>,<glob2>,...,<globn>}`` matches any string that is matched
   by one of ``<glob1>``, ``<glob2>``, etc.

.. list-table:: Glob syntax examples
   :header-rows: 1

   -  -  Syntax
      -  Files matched
      -  Files not matched

   -  -  ``x``
      -  ``x``
      -  ``y``

   -  -  ``\*``
      -  ``*``
      -  ``x``

   -  -  ``file*.txt``
      -  ``file1.txt``, ``file2.txt``
      -  ``f.txt``

   -  -  ``*.txt``
      -  ``f.txt``
      -  ``.hidden.txt``

   -  -  ``a**``
      -  ``aml``
      -  ``a.ml``

   -  -  ``**``
      -  ``a/b``, ``a.b``
      -  (none)

   -  -  ``a?.txt``
      -  ``a1.txt``, ``a2.txt``
      -  ``b1.txt``, ``a10.txt``

   -  -  ``f[xyz].txt``
      -  ``fx.txt``, ``fy.txt``, ``fz.txt``
      -  ``f2.txt``, ``f.txt``

   -  -  ``f[!xyz].txt``
      -  ``f2.txt``, ``fa.txt``
      -  ``fx.txt``, ``f.txt``

   -  -  ``a.{ml,mli}``
      -  ``a.ml``, ``a.mli``
      -  ``a.txt``, ``b.ml``

   -  -  ``../a.{ml,mli}``
      -  ``../a.ml``, ``../a.mli``
      -  ``a.ml``
