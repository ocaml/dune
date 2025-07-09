lock_dir
========

.. warning::

   :doc:`Dune Package Management </explanation/package-management>` is not
   final yet and the configuration options are subject to change.

This stanza configures the lock directory settings for the current workspace.
For the default workflow no configuration is necessary, but the defaults can be
changed if desired.

.. describe:: (lock_dir ...)

   .. versionadded:: 3.13

   Configures a specific lock directory to be created or used.

   .. describe:: (path <string>)

      The location in the source tree where the lock directory will be
      created or read from. If not specified defaults to ``dune.lock``.

   .. describe:: (repositories <name list>)

      The repositories to be used for finding a package solution, specified
      in priority order. Supports ``:standard`` which contains ``upstream`` and
      ``overlay``.

      Additional repositories can be defined using the
      :doc:`/reference/dune-workspace/repository` stanza.

   .. describe:: (solver_env ...)

      The environment that is injected into the solver when creating the lock
      directory.

      It consists of a sequence of ``(<name> <value>)`` pairs.

   .. describe:: (unset_variables <name list>)

      A list of variables that are used in solving that are deliberately unset
      even if the solver could provide bindings for them.

      The variables here cannot overlap with those defined in ``solver_env``.

   .. describe:: (pins <name list>)

      .. versionadded:: 3.15

      Define which pins are enabled for this particular lock dir. See
      :doc:`/reference/dune-workspace/pin` for details on how to define pins.

   .. describe:: (version_preference <string>)

      Can be one of:

      - ``newest`` (default): The solver will pick the newest available
         version of a package that satisfies the constraints.
      - ``oldest``: The solver will pick the lowest version that will satisfy
         the constraints

   .. describe:: (constraints <dep-specification>)

      Adds additional solver constraints that are passed to the solver. Follows
      the :token:`~pkg-dep:dep_specification` format.

      .. note::

         Names introduced through ``constraints`` are not considered
         dependencies and not added to the lockfile. They exist solely to add
         additional constraints if the packages to which the constraint is
         applied are selected and don't do anything otherwise.

   .. describe:: (depopts <name list>)

      .. versionadded:: 3.19

      Defines which optional packages names (``depopts``) the solver should
      include when attempting to find a solution for the project.
