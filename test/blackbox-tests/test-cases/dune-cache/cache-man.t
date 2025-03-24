Here we observe the documentation for the dune cache commands.

  $ dune cache --help=plain
  NAME
         dune-cache - Manage Dune's shared cache of build artifacts.
  
  SYNOPSIS
         dune cache COMMAND …
  
  DESCRIPTION
         Dune can share build artifacts between workspaces. We currently only
         support a few subcommands; however, we plan to provide more
         functionality soon.
  
  COMMANDS
         clear [OPTION]…
             Clear the Dune cache.
  
         size [--machine-readable] [OPTION]…
             Query the size of the Dune cache.
  
         trim [--size=BYTES] [--trimmed-size=BYTES] [OPTION]…
             Trim the Dune cache.
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         dune cache exits with:
  
         0   on success.
  
         1   if an error happened.
  
         130 if it was interrupted by a signal.
  
  SEE ALSO
         dune(1)
  
Testing the output of `dune cache size --machine-readable`

  $ dune cache size --help=plain
  NAME
         dune-cache-size - Query the size of the Dune cache.
  
  SYNOPSIS
         dune cache size [--machine-readable] [OPTION]…
  
         Compute the total size of files in the Dune cache which are not
         hardlinked from any build directory and output it in a human-readable
         form.
  
  OPTIONS
         --machine-readable
             Outputs size as a plain number of bytes.
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         dune cache size exits with:
  
         0   on success.
  
         1   if an error happened.
  
         130 if it was interrupted by a signal.
  
  SEE ALSO
         dune(1)
  
Testing the output of dune cache trim.

  $ dune cache trim --help=plain
  NAME
         dune-cache-trim - Trim the Dune cache.
  
  SYNOPSIS
         dune cache trim [--size=BYTES] [--trimmed-size=BYTES] [OPTION]…
  
         Trim the Dune cache to a specified size or by a specified amount.
  
  OPTIONS
         --size=BYTES
             Size to trim the cache to. BYTES is the number of bytes followed
             by a unit. Byte units can be one of B, kB, KiB, MB, MiB, GB, GiB,
             TB or TiB.
  
         --trimmed-size=BYTES
             Size to trim from the cache. BYTES is the same as for --size.
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         dune cache trim exits with:
  
         0   on success.
  
         1   if an error happened.
  
         130 if it was interrupted by a signal.
  
  EXAMPLES
         Trimming the Dune cache to 1 GB.
         
                    $ dune cache trim --size=1GB 
  
         Trimming 500 MB from the Dune cache.
         
                    $ dune cache trim --trimmed-size=500MB 
  
  SEE ALSO
         dune(1)
  
