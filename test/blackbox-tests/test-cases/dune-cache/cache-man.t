Here we observe the documentation for the dune cache commands.

  $ dune cache --help=plain
  NAME
         dune-cache - Manage the shared cache of build artifacts
  
  SYNOPSIS
         dune cache COMMAND …
  
  DESCRIPTION
         Dune can share build artifacts between workspaces. Currently, the only
         action supported by this command is `trim`, but we plan to provide
         more functionality soon.
  
  COMMANDS
         trim [--size=BYTES] [--trimmed-size=BYTES] [OPTION]…
             Trim the Dune cache
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         cache exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  SEE ALSO
         dune(1)
  

Man pages of the deprecated start and stop commands.

  $ dune cache start --help=plain
  NAME
         dune-cache - Manage the shared cache of build artifacts
  
  SYNOPSIS
         dune cache COMMAND …
  
  DESCRIPTION
         Dune can share build artifacts between workspaces. Currently, the only
         action supported by this command is `trim`, but we plan to provide
         more functionality soon.
  
  COMMANDS
         trim [--size=BYTES] [--trimmed-size=BYTES] [OPTION]…
             Trim the Dune cache
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         cache exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  SEE ALSO
         dune(1)
  

  $ dune cache stop --help=plain
  NAME
         dune-cache - Manage the shared cache of build artifacts
  
  SYNOPSIS
         dune cache COMMAND …
  
  DESCRIPTION
         Dune can share build artifacts between workspaces. Currently, the only
         action supported by this command is `trim`, but we plan to provide
         more functionality soon.
  
  COMMANDS
         trim [--size=BYTES] [--trimmed-size=BYTES] [OPTION]…
             Trim the Dune cache
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         cache exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  SEE ALSO
         dune(1)
  

Testing the output of dune cache trim.

  $ dune cache trim --help=plain
  NAME
         dune-cache-trim - Trim the Dune cache
  
  SYNOPSIS
         dune cache trim [--size=BYTES] [--trimmed-size=BYTES] [OPTION]…
  
         Trim the Dune cache to a specified size or by a specified amount.
  
  OPTIONS
         --size=BYTES
             Size to trim the cache to.
  
         --trimmed-size=BYTES
             Size to trim from the cache.
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         trim exits with the following status:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  EXAMPLES
         Trimming the Dune cache to 1 GB.
         
                    $ dune cache trim --trimmed-size=1GB 
  
         Trimming 500 MB from the Dune cache.
         
                    $ dune cache trim --size=500MB 
  
  SEE ALSO
         dune(1)
  
