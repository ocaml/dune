Validation of target field in melange.emit stanzas

  $ make_melange_project 3.8 0.1

Target should not be empty

  $ lib=foo
  $ cat > dune <<EOF
  > (library
  >  (name $lib)
  >  (modes melange))
  > (melange.emit
  >  (target "")
  >  (emit_stdlib false)
  >  (libraries $lib)
  >  (module_system es6))
  > EOF

  $ dune build
  File "dune", line 5, characters 9-11:
  5 |  (target "")
               ^^
  Error: The field target can not be empty
  [1]

Special directory names currently bypass target validation and cause an
internal error during rule generation.

  $ cat > dune <<EOF
  > (library
  >  (name $lib)
  >  (modes melange))
  > (melange.emit
  >  (target .)
  >  (emit_stdlib false)
  >  (libraries $lib))
  > EOF

  $ dune build 2>&1 | sed '/^Raised at/,$d'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Filename.of_string_exn: invalid filename", { filename = "." })
  [1]

  $ cat > dune <<EOF
  > (library
  >  (name $lib)
  >  (modes melange))
  > (melange.emit
  >  (target ..)
  >  (emit_stdlib false)
  >  (libraries $lib))
  > EOF

  $ dune build 2>&1 | sed '/^Raised at/,$d'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Filename.of_string_exn: invalid filename", { filename = ".." })
  [1]

Target should not try to descend into subdirectories

  $ cat > dune <<EOF
  > (library
  >  (name $lib)
  >  (modes melange))
  > (melange.emit
  >  (target foo/bar)
  >  (emit_stdlib false)
  >  (libraries $lib)
  >  (module_system es6))
  > EOF

  $ dune build
  File "dune", line 5, characters 9-16:
  5 |  (target foo/bar)
               ^^^^^^^
  Error: The field target must use simple names and can not include paths to
  other folders. To emit JavaScript files in another folder, move the
  `melange.emit` stanza to that folder
  [1]

Target should not try to escape into parent directories

  $ rm dune
  $ mkdir bar
  $ mkdir foo
  $ cat > bar/dune <<EOF
  > (library
  >  (name bar)
  >  (modes melange))
  > EOF

  $ cat > foo/dune <<EOF
  > (library
  >  (name $lib)
  >  (modes melange))
  > (melange.emit
  >  (target ../bar)
  >  (emit_stdlib false)
  >  (libraries $lib)
  >  (module_system es6))
  > EOF

  $ dune build
  File "foo/dune", line 5, characters 9-15:
  5 |  (target ../bar)
               ^^^^^^
  Error: The field target must use simple names and can not include paths to
  other folders. To emit JavaScript files in another folder, move the
  `melange.emit` stanza to that folder
  [1]
