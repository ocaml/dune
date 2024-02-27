########
 config
########

This file is used to set Dune's global configuration, which is
applicable across projects and workspaces.

The configuration file is normally ``~/.config/dune/config`` on Unix
systems and ``%LOCALAPPDATA%/dune/config`` on Windows. However, for most
Dune commands, it is possible to specify an alternative configuration
file with the ``--config-file`` option. Command-line flags take
precedence over the contents of the ``config`` file. If ``--no-config``
or ``-p`` is passed, Dune will not read this file.

The ``config`` file can contain the following stanzas:

.. toctree::

   action_stdout_on_success
   action_stderr_on_success
   cache
   cache_check_probability
   cache_storage_mode
   display
   jobs
   sandboxing_preference
   terminal_persistence
