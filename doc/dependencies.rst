.. _deps-field:

************************
Dependency specification
************************

Dependencies in ``dune`` files can be specified using one of the following:

.. _source_tree:

- ``(:name <dependencies>)`` will bind the the list of dependencies to the
  ``name`` variable. This variable will be available as ``%{name}`` in actions.
- ``(file <filename>)`` or simply ``<filename>``: depend on this file
- ``(alias <alias-name>)``: depend on the construction of this alias, for
  instance: ``(alias src/runtest)``
- ``(alias_rec <alias-name>)``: depend on the construction of this
  alias recursively in all children directories wherever it is
  defined. For instance: ``(alias_rec src/runtest)`` might depend on
  ``(alias src/runtest)``, ``(alias src/foo/bar/runtest)``, ...
- ``(glob_files <glob>)``: depend on all files matched by ``<glob>``, see the
  :ref:`glob <glob>` for details
- ``(source_tree <dir>)``: depend on all source files in the subtree with root
  ``<dir>``

- ``(universe)``: depend on everything in the universe. This is for
  cases where dependencies are too hard to specify. Note that dune
  will not be able to cache the result of actions that depend on the
  universe. In any case, this is only for dependencies in the
  installed world, you must still specify all dependencies that come
  from the workspace.
- ``(package <pkg>)`` depend on all files installed by ``<package>``, as well
  as on the transitive package dependencies of ``<package>``. This can be used
  to test a command against the files that will be installed
- ``(env_var <var>)``: depend on the value of the environment variable ``<var>``.
  If this variable becomes set, becomes unset, or changes value, the target
  will be rebuilt.
- ``(sandbox <config>)``: require a particular sandboxing configuration.
  Config can be one (or many) of:
  - ``always``: the action requires a clean environment.
  - ``none``: the action must run in the build directory.
  - ``preserve_file_kind``: the action needs the files it reads to look
  like normal files (so dune won't use symlinks for sandboxing)

In all these cases, the argument supports :ref:`variables`.

Named Dependencies
~~~~~~~~~~~~~~~~~~

dune allows a user to organize dependency lists by naming them. The user is
allowed to assign a group of dependencies a name that can later be referred to
in actions (like the ``%{deps}``, ``%{target}`` and ``%{targets}`` built in variables).

One instance where this is useful is for naming globs. Here's an
example of an imaginary bundle command:

.. code:: scheme

   (rule
    (target archive.tar)
    (deps
     index.html
     (:css (glob_files *.css))
     (:js foo.js bar.js)
     (:img (glob_files *.png) (glob_files *.jpg)))
    (action
     (run %{bin:bundle} index.html -css %{css} -js %{js} -img %{img} -o %{target})))

Note that such named dependency list can also include unnamed
dependencies (like ``index.html`` in the example above). Also, such
user defined names will shadow built in variables. So
``(:workspace_root x)`` will shadow the built in ``%{workspace_root}``
variable.

.. _glob:

Glob
~~~~

You can use globs to declare dependencies on a set of files. Note that globs
will match files that exist in the source tree as well as buildable targets, so
for instance you can depend on ``*.cmi``.

Currently dune only support globbing files in a single directory. And in
particular the glob is interpreted as follows:

- anything before the last ``/`` is taken as a literal path
- anything after the last ``/``, or everything if the glob contains no ``/``, is
  interpreted using the glob syntax

The glob syntax is interpreted as follows:

- ``\<char>`` matches exactly ``<char>``, even if it is a special character
  (``*``, ``?``, ...)
- ``*`` matches any sequence of characters, except if it comes first in which
  case it matches any character that is not ``.`` followed by anything
- ``**`` matches any character that is not ``.`` followed by anything, except if
  it comes first in which case it matches anything
- ``?`` matches any single character
- ``[<set>]`` matches any character that is part of ``<set>``
- ``[!<set>]`` matches any character that is not part of ``<set>``
- ``{<glob1>,<glob2>,...,<globn>}`` matches any string that is matched by one of
  ``<glob1>``, ``<glob2>``, ...
