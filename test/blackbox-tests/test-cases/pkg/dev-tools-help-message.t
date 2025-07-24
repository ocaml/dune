Exercise printing the help messages for dev tools commands.

Disable the pagers, as the command line parser might use a pager from the users
system which will reformat the output if not specifically requesting `plain`
output.

  $ export PAGER=false
  $ export MANPAGER=false

Output the help text:

  $ dune tools exec --help=plain
  NAME
         dune-tools-exec - Command group for running wrapped tools.
  
  SYNOPSIS
         dune tools exec COMMAND …
  
  COMMANDS
         ocamlearlybird [OPTION]… [ARGS]…
             Wrapper for running ocamlearlybird intended to be run
             automatically by a text editor. All positional arguments will be
             passed to the ocamlearlybird executable (pass flags to
             ocamlearlybird after the '--' argument, such as 'dune tools exec
             ocamlearlybird -- --help').
  
         ocamlformat [OPTION]… [ARGS]…
             Wrapper for running ocamlformat intended to be run automatically
             by a text editor. All positional arguments will be passed to the
             ocamlformat executable (pass flags to ocamlformat after the '--'
             argument, such as 'dune tools exec ocamlformat -- --help').
  
         ocamllsp [OPTION]… [ARGS]…
             Wrapper for running ocamllsp intended to be run automatically by a
             text editor. All positional arguments will be passed to the
             ocamllsp executable (pass flags to ocamllsp after the '--'
             argument, such as 'dune tools exec ocamllsp -- --help').
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
         --version
             Show version information.
  
  EXIT STATUS
         dune tools exec exits with:
  
         0   on success.
  
         1   if an error happened.
  
         130 if it was interrupted by a signal.
  
  SEE ALSO
         dune(1)
  
