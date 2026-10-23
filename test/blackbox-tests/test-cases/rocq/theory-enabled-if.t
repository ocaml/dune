A disabled rocq.theory must not declare its rocqdoc directory targets nor
install its files.

  $ make_rocq_project 3.24 0.14
  $ cat > a.v <<EOF
  > Definition x := 1.
  > EOF

A theory disabled with (enabled_if false):

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (enabled_if false))
  > EOF

  $ dune build 2>&1 | awk '/Internal error/,/Raised/'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("gen_rules returned a set of directory targets that doesn't match the set of directory targets from returned rules",
     { dir = In_build_dir "default"
     ; mismatched_directories =
         map
           { "default/Foo.html" : { message = "not generated"; loc = "dune:1" }
           ; "default/Foo.tex" : { message = "not generated"; loc = "dune:1" }
           }
     })
  Raised at Stdune__Code_error.raise in file
  [1]
  $ dune build @install 2>&1 | awk '/Internal error/,/Raised/'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("gen_rules returned a set of directory targets that doesn't match the set of directory targets from returned rules",
     { dir = In_build_dir "default"
     ; mismatched_directories =
         map
           { "default/Foo.html" : { message = "not generated"; loc = "dune:1" }
           ; "default/Foo.tex" : { message = "not generated"; loc = "dune:1" }
           }
     })
  Raised at Stdune__Code_error.raise in file
  [1]
  $ ls _build/default/*.vo
  ls: cannot access '_build/default/*.vo': No such file or directory
  [2]

The same theory, enabled or disabled through an environment variable:

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (enabled_if %{env:FOO_ENABLED=false}))
  > EOF

  $ dune build 2>&1 | awk '/Internal error/,/Raised/'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("gen_rules returned a set of directory targets that doesn't match the set of directory targets from returned rules",
     { dir = In_build_dir "default"
     ; mismatched_directories =
         map
           { "default/Foo.html" : { message = "not generated"; loc = "dune:1" }
           ; "default/Foo.tex" : { message = "not generated"; loc = "dune:1" }
           }
     })
  Raised at Stdune__Code_error.raise in file
  [1]
  $ ls _build/default/*.vo
  ls: cannot access '_build/default/*.vo': No such file or directory
  [2]

  $ FOO_ENABLED=true dune build
  $ ls _build/default/*.vo
  _build/default/a.vo

  $ FOO_ENABLED=true dune build Foo.html/
  $ ls _build/default/Foo.html
  Foo.a.html
  coqdoc.css
  index.html
  toc.html

A disabled theory attached to a package is not installed. As for a disabled
library, a package without any other stanza is then reported as empty:

  $ cat >> dune-project <<EOF
  > (package (name foo))
  > EOF
  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (package foo)
  >  (enabled_if %{env:FOO_ENABLED=false}))
  > EOF

  $ dune build @install 2>&1 | awk '/Internal error/,/Raised/'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("gen_rules returned a set of directory targets that doesn't match the set of directory targets from returned rules",
     { dir = In_build_dir "default"
     ; mismatched_directories =
         map
           { "default/Foo.html" : { message = "not generated"; loc = "dune:1" }
           ; "default/Foo.tex" : { message = "not generated"; loc = "dune:1" }
           }
     })
  Raised at Stdune__Code_error.raise in file
  [1]

  $ FOO_ENABLED=true dune build @install
  $ grep 'a\.vo' _build/default/foo.install
    "_build/install/default/lib/coq/user-contrib/Foo/a.vo" {"coq/user-contrib/Foo/a.vo"}

With (allow_empty), the package installs nothing from the disabled theory:

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using rocq 0.14)
  > (package (name foo) (allow_empty))
  > EOF

  $ dune build @install 2>&1 | awk '/Internal error/,/Raised/'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("gen_rules returned a set of directory targets that doesn't match the set of directory targets from returned rules",
     { dir = In_build_dir "default"
     ; mismatched_directories =
         map
           { "default/Foo.html" : { message = "not generated"; loc = "dune:1" }
           ; "default/Foo.tex" : { message = "not generated"; loc = "dune:1" }
           }
     })
  Raised at Stdune__Code_error.raise in file
  [1]
  $ grep 'a\.vo' _build/default/foo.install
    "_build/install/default/lib/coq/user-contrib/Foo/a.vo" {"coq/user-contrib/Foo/a.vo"}
