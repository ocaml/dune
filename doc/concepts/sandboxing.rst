############
 Sandboxing
############

..
   TODO(diataxis)
   - explanation: sandboxing
   - reference: sandboxing

The user actions that run external commands (``run``, ``bash``,
``system``) are opaque to Dune, so Dune has to rely on manual
specification of dependencies and targets. One problem with manual
specification is that it's error-prone. It's often hard to know in
advance what files the command will read, and knowing a correct set of
dependencies is very important for build reproducibility and incremental
build correctness.

To help with this problem Dune supports sandboxing. An idealized view of
sandboxing is that it runs the action in an environment where it can't
access anything except for its declared dependencies.

In practice, we have to make compromises and have some trade-offs
between simplicity, information leakage, performance, and portability.

The way sandboxing is currently implemented is that for each sandboxed
action we build a separate directory tree (sandbox directory) that
mirrors the build directory, filtering it to only contain the files that
were declared as dependencies. We run the action in that directory, and
then we copy the targets back to the build directory.

You can configure Dune to use sandboxing modes ``symlink``,
``hardlink``, or ``copy``, which determine how the individual files are
populated (they will be symlinked, hardlinked, or copied into the
sandbox directory).

This approach is very simple and portable, but that comes with certain
limitations:

-  The actions in the sandbox can use absolute paths to refer to
   anywhere outside the sandbox. This means that only dependencies on
   relative paths in the build tree can be enforced/detected by
   sandboxing.

-  The sandboxed actions still run with full permissions of Dune itself,
   so sandboxing is not a security feature. It won't prevent network
   access either.

-  We don't erase the environment variables of the sandboxed commands.
   This is something we want to change.

-  Performance impact is usually small, but it can get noticeable for
   fast actions with very large sets of dependencies.

*************************************
 Per-Action Sandboxing Configuration
*************************************

Some actions may rely on sandboxing to work correctly. For example, an
action may need the input directory to contain nothing except the input
files, or the action might create temporary files that break other build
actions.

Some other actions may refuse to work with Sandboxing. For example, if
they rely on absolute path to the build directory staying fixed, or if
they deliberately use some files without declaring dependencies (this is
usually a very bad idea, by the way).

Generally it's better to improve the action so it works with or without
sandboxing (especially with), but sometimes you just can't do that.

Things like this can be described using the "sandbox" field in the
dependency specification language (see :doc:`dependency-spec`).

*********************************
 Global Sandboxing Configuration
*********************************

Dune always respects per-action sandboxing specification. You can
configure it globally to prefer a certain sandboxing mode if the action
allows it.

This is controlled by:

-  ``dune --sandbox <...>`` CLI flag (see ``man dune-build``)
-  ``DUNE_SANDBOX`` environment (see ``man dune-build``)
-  ``(sandboxing_preference ..)`` field in the configuration file (see
   ``man dune-config``)
