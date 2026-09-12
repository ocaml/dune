Custom toplevel names are currently not validated when parsing the stanza.

  $ make_dune_project 3.21

A name containing a path causes an internal error.

  $ cat >dune <<EOF
  > (toplevel
  >  (name ../foo))
  > EOF

  $ dune build 2>&1 | sed '/^Raised at/,$d'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Filename.of_string_exn: invalid filename", { filename = "../foo.ml-gen" })
  [1]

An empty name is currently accepted.

  $ cat >dune <<EOF
  > (toplevel
  >  (name ""))
  > EOF

  $ dune build
