When `--destdir` is add, the destination is know and all others must be relative.

  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam manfile.1
  $ cat >dune <<EOF
  > (install
  >  (section man)
  >  (files manfile.1))
  > EOF
  $ dune build @install
  $ rm -fr dest-dir
  $ mkdir dest-dir
  $ dune install --prefix foo --destdir $PWD/dest-dir --mandir share/man 
  $ tree dest-dir -a
  dest-dir
  `-- foo
      |-- lib
      |   `-- foo
      |       |-- META
      |       |-- dune-package
      |       `-- opam
      `-- share
          `-- man
              `-- man1
                  `-- manfile.1
  
  6 directories, 4 files

Fails because `--mandir` must be absolute, in absence of `--destdir` we must avoid ambiguity.
  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam manfile.1
  $ cat >dune <<EOF
  > (install
  >  (section man)
  >  (files manfile.1))
  > EOF
  $ dune build @install
  $ rm -fr dest-dir
  $ mkdir dest-dir
  $ dune install --prefix $PWD/dest-dir --mandir share/man 
  Error:
  Option '--mandir' the path must be absolute to avoid ambiguity or add
  '--destdir'
  [1]
  $ tree dest-dir -a
  dest-dir
  
  0 directories, 0 files

Fails because `--mandir` is absolute when `--destdir` exists.
  $ echo "(lang dune 2.0)" > dune-project
  $ touch foo.opam manfile.1
  $ cat >dune <<EOF
  > (install
  >  (section man)
  >  (files manfile.1))
  > EOF
  $ dune build @install
  $ rm -fr dest-dir
  $ mkdir dest-dir
  $ dune install --prefix foo --destdir $PWD/dest-dir --mandir $PWD/dest-dir/share/man 
  Error:
  Option '--mandir' must to be relative because the destination '--destdir' is
  known
  [1]
  $ tree dest-dir -a
  dest-dir
  
  0 directories, 0 files
