The bubblewrap CLI can be checked without creating a user namespace.

  $ dune_bin=$(command -v dune)
  $ "$dune_bin" internal with-bwrap
  Error: missing command after --
  [1]

A missing backend is reported without attempting to run the command.

  $ mkdir no-bwrap
  $ PATH="$PWD/no-bwrap" "$dune_bin" internal with-bwrap -- unused
  Error: unable to find bwrap in PATH
  [1]

A failing backend receives the command's argument boundaries intact, and its
exit status is preserved. The fake backend must not execute the command.

  $ cat > no-bwrap/bwrap <<'EOF'
  > #!/bin/sh
  > while [ "$1" != -- ]; do shift; done
  > shift
  > printf '[%s]\n' "$@"
  > exit 17
  > EOF
  $ chmod +x no-bwrap/bwrap
  $ PATH="$PWD/no-bwrap" "$dune_bin" internal with-bwrap -- \
  >   unused 'two words' 'quote"and\slash'
  [unused]
  [two words]
  [quote"and\slash]
  [17]
