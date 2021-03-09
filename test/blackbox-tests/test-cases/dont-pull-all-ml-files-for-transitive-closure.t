This test checks that when an executable doesn't depend on an .ml
file, Dune won't even look at the .ml file. This is important for:

- executables stanzas with multiple executables. If we build only one
of the executables, Dune shouldn't care if .ml files not pulled by
this exe don't parse

- for the upcoming ctypes rules where we build several intermediate
executables out of the same compilation context

  $ echo '(lang dune 2.8)' > dune-project
  $ cat >dune<<EOF
  > (executables (names x y))
  > EOF
  $ cat >x.ml<<EOF
  > print_endline "Hello, world!"
  > EOF
  $ cat >y.ml<<EOF
  > (* unclosed comment
  > EOF
  $ cp y.ml z.ml

At the moment, this doesn't work; dune still try to parse ml files
that are not used:

  $ dune exec ./x.exe
  Hello, world!
