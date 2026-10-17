Sandbox a rule that depends on a directory target using the copying sandbox
mode:

  $ umask 022

  $ make_directory_targets_project 3.8

  $ cat >print-permissions.sh <<EOF
  > /usr/bin/env sh
  > echo permissions of output/
  > dune_cmd stat permissions output
  > echo permissions of output/y
  > dune_cmd stat permissions output/y
  > echo permissions of output/x
  > dune_cmd stat permissions output/x
  > echo permissions of output/subdir
  > dune_cmd stat permissions output/subdir
  > echo permissions of output/subdir/z
  > dune_cmd stat permissions output/subdir/z
  > echo ""
  > EOF

  $ chmod +x print-permissions.sh

  $ cat >dune <<EOF
  > (rule
  >  (deps print-permissions.sh)
  >  (targets (dir output))
  >  (action (system "\| echo building output/
  >                  "\| mkdir -p output/subdir
  >                  "\| touch output/x output/y output/subdir/z
  >                  "\| chmod 0777 output/
  >                  "\| chmod 0444 output/y
  >                  "\| chmod 0776 output/subdir
  >                  "\| chmod 0666 output/subdir/z
  >                  "\| . ./print-permissions.sh
  >          )))
  > (rule
  >  (target foo)
  >  (deps print-permissions.sh (sandbox always) output/)
  >  (action (system "\| . ./print-permissions.sh
  >                  "\| touch foo;
  >          )))
  > EOF

  $ dune build foo --sandbox=copy
  building output/
  permissions of output/
  777
  permissions of output/y
  444
  permissions of output/x
  644
  permissions of output/subdir
  776
  permissions of output/subdir/z
  666
  
  permissions of output/
  755
  permissions of output/y
  644
  permissions of output/x
  644
  permissions of output/subdir
  754
  permissions of output/subdir/z
  644
  

  $ ( cd _build/default && ../../print-permissions.sh )
  permissions of output/
  777
  permissions of output/y
  444
  permissions of output/x
  444
  permissions of output/subdir
  776
  permissions of output/subdir/z
  444
  

Now we demonstrate that symlinks aren't supported:

  $ mkdir sub

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (echo test))
  >  (deps sub/targetdir))
  > EOF

  $ runtest() {
  > cat >sub/dune <<EOF
  > (rule
  >  (target (dir targetdir))
  >  (action (system "mkdir targetdir && cd targetdir && $1")))
  > EOF
  > dune build @foo --sandbox=copy
  > }

  $ runtest "touch foo && ln -s foo bar"
  test

  $ runtest "mkdir bar && touch bar/somefileinbar && ln -s bar symlinktobar"
  File "sub/dune", lines 1-3, characters 0-151:
  1 | (rule
  2 |  (target (dir targetdir))
  3 |  (action (system "mkdir targetdir && cd targetdir && mkdir bar && touch bar/somefileinbar && ln -s bar symlinktobar")))
  Error: Error trying to read targets after a rule was run:
  - sub/targetdir/symlinktobar: Unexpected file kind "S_DIR" (directory)
  [1]

Now we try a broken symlink:

  $ runtest "ln -s foo bar"
  File "sub/dune", lines 1-3, characters 0-102:
  1 | (rule
  2 |  (target (dir targetdir))
  3 |  (action (system "mkdir targetdir && cd targetdir && ln -s foo bar")))
  Error: Error trying to read targets after a rule was run:
  - sub/targetdir/bar: Broken symbolic link
  [1]

In symlink mode, materialize directory inputs as real directories containing
file symlinks. A directory and its explicit descendants can overlap. Replacing
or deleting entries must not modify the original directory target.

  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir input))
  >  (action
  >   (system "\| mkdir -p input/sub/deep input/sub/empty input/empty
  >           "\| echo outer > input/a
  >           "\| echo inner > input/sub/deep/b
  >           "\| ln -s sub/deep/b input/link
  >   )))
  > (rule
  >  (target result)
  >  (deps
  >   (sandbox always) input input/sub input/sub/deep/b
  >   (glob_files input/empty/*))
  >  (action
  >   (with-stdout-to result
  >    (system "\| if test -L input || test -L input/sub || test -L input/sub/deep; then
  >            "\|   echo 'directory symlink'
  >            "\| else
  >            "\|   test -L input/a && test -L input/sub/deep/b && test -L input/link || exit 1
  >            "\|   test -d input/empty && test -d input/sub/empty || exit 1
  >            "\|   cat input/a input/sub/deep/b input/link
  >            "\|   rm input/sub/deep/b || exit 1
  >            "\|   echo replacement > input/sub/deep/b || exit 1
  >            "\|   rmdir input/empty || exit 1
  >            "\|   mkdir input/local || exit 1
  >            "\|   test ! -e input/empty && test -d input/local || exit 1
  >            "\|   cat input/sub/deep/b
  >            "\| fi
  >    ))))
  > EOF

  $ dune build result --sandbox symlink
  $ cat _build/default/result
  outer
  inner
  inner
  replacement

  $ cat _build/default/input/sub/deep/b
  inner
  $ test -d _build/default/input/empty
  $ test ! -e _build/default/input/local
