Here we observe the documentation for the dune cache commands.

  $ dune cache --help=plain
  NAME
         dune-cache - Manage the shared cache of build artifacts
  
  SYNOPSIS
         dune cache [--size=BYTES] [--trimmed-size=BYTES] [OPTION]… [ACTION]
  
  DESCRIPTION
         Dune can share build artifacts between workspaces. Currently, the only
         action supported by this command is `trim`, but we plan to provide
         more functionality soon. 
  
  ACTIONS
         trim trim the shared cache to free space.
  
  ARGUMENTS
         ACTION
             The cache action to perform (trim)
  
  OPTIONS
         --size=BYTES
             Size to trim the cache to.
  
         --trimmed-size=BYTES
             Size to trim from the cache.
  
  EXAMPLES
         Trimming the Dune cache to 1 GB.
                 
                 $ dune cache trim --trimmed-size=1GB 
  
         Trimming 500 MB from the Dune cache.
                 
                 $ dune cache trim --size=500MB 
  
  COMMON OPTIONS
         These options are common to all commands.
  
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  MORE HELP
         Use `dune COMMAND --help' for help on a single command.
  
  EXIT STATUS
         cache exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  BUGS
         Check bug reports at https://github.com/ocaml/dune/issues
  
  SEE ALSO
         dune(1)
  
Test the error message when using removed subcommands [start] and [stop].

  $ dune cache start
  Error: Dune no longer uses the cache daemon, and so the `start` and `stop`
  subcommands of `dune cache` were removed.
  [1]

  $ dune cache stop
  Error: Dune no longer uses the cache daemon, and so the `start` and `stop`
  subcommands of `dune cache` were removed.
  [1]

Man pages of the deprecated start and stop commands.

  $ dune cache start --help=plain
  NAME
         dune-cache - Manage the shared cache of build artifacts
  
  SYNOPSIS
         dune cache [--size=BYTES] [--trimmed-size=BYTES] [OPTION]… [ACTION]
  
  DESCRIPTION
         Dune can share build artifacts between workspaces. Currently, the only
         action supported by this command is `trim`, but we plan to provide
         more functionality soon. 
  
  ACTIONS
         trim trim the shared cache to free space.
  
  ARGUMENTS
         ACTION
             The cache action to perform (trim)
  
  OPTIONS
         --size=BYTES
             Size to trim the cache to.
  
         --trimmed-size=BYTES
             Size to trim from the cache.
  
  EXAMPLES
         Trimming the Dune cache to 1 GB.
                 
                 $ dune cache trim --trimmed-size=1GB 
  
         Trimming 500 MB from the Dune cache.
                 
                 $ dune cache trim --size=500MB 
  
  COMMON OPTIONS
         These options are common to all commands.
  
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  MORE HELP
         Use `dune COMMAND --help' for help on a single command.
  
  EXIT STATUS
         cache exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  BUGS
         Check bug reports at https://github.com/ocaml/dune/issues
  
  SEE ALSO
         dune(1)
  

  $ dune cache stop --help=plain
  NAME
         dune-cache - Manage the shared cache of build artifacts
  
  SYNOPSIS
         dune cache [--size=BYTES] [--trimmed-size=BYTES] [OPTION]… [ACTION]
  
  DESCRIPTION
         Dune can share build artifacts between workspaces. Currently, the only
         action supported by this command is `trim`, but we plan to provide
         more functionality soon. 
  
  ACTIONS
         trim trim the shared cache to free space.
  
  ARGUMENTS
         ACTION
             The cache action to perform (trim)
  
  OPTIONS
         --size=BYTES
             Size to trim the cache to.
  
         --trimmed-size=BYTES
             Size to trim from the cache.
  
  EXAMPLES
         Trimming the Dune cache to 1 GB.
                 
                 $ dune cache trim --trimmed-size=1GB 
  
         Trimming 500 MB from the Dune cache.
                 
                 $ dune cache trim --size=500MB 
  
  COMMON OPTIONS
         These options are common to all commands.
  
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  MORE HELP
         Use `dune COMMAND --help' for help on a single command.
  
  EXIT STATUS
         cache exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  BUGS
         Check bug reports at https://github.com/ocaml/dune/issues
  
  SEE ALSO
         dune(1)
  

Testing the output of dune cache trim.

  $ dune cache trim --help=plain
  NAME
         dune-cache - Manage the shared cache of build artifacts
  
  SYNOPSIS
         dune cache [--size=BYTES] [--trimmed-size=BYTES] [OPTION]… [ACTION]
  
  DESCRIPTION
         Dune can share build artifacts between workspaces. Currently, the only
         action supported by this command is `trim`, but we plan to provide
         more functionality soon. 
  
  ACTIONS
         trim trim the shared cache to free space.
  
  ARGUMENTS
         ACTION
             The cache action to perform (trim)
  
  OPTIONS
         --size=BYTES
             Size to trim the cache to.
  
         --trimmed-size=BYTES
             Size to trim from the cache.
  
  EXAMPLES
         Trimming the Dune cache to 1 GB.
                 
                 $ dune cache trim --trimmed-size=1GB 
  
         Trimming 500 MB from the Dune cache.
                 
                 $ dune cache trim --size=500MB 
  
  COMMON OPTIONS
         These options are common to all commands.
  
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  MORE HELP
         Use `dune COMMAND --help' for help on a single command.
  
  EXIT STATUS
         cache exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  BUGS
         Check bug reports at https://github.com/ocaml/dune/issues
  
  SEE ALSO
         dune(1)
  
