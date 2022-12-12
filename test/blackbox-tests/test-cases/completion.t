  $ dune complete script > complete.sh

`test` calls `_dune` defined in `complete.sh`. Usually, bash itself would fill
`COMP_*` but we have to do that by hand.

The convention is that if the command line does not end with a space, we're
completing the last word. Otherwise we're completing the next word.

Examples: `test 'dune '` corresponds to 'dune <tab>' (space, tab). We expect to
see a list of commands. `test 'dune bui'` corresponds to 'dune bui<tab>'. We
expect to see the list of commands that start with 'bui' (ie, just "build")

  $ cat > test << 'EOF'
  > #!/usr/bin/env bash
  > COMP_LINE=$1
  > 
  > . complete.sh
  > 
  > COMP_WORDS=( $COMP_LINE )
  > COMP_POINT=${#COMP_LINE}
  > COMP_CWORD=$(( ${#COMP_WORDS[@]} - 1 ))
  > [[ $COMP_LINE =~ .*" "$ ]] && ((++COMP_CWORD))
  > 
  > _dune
  > 
  > printf '%s\n' "${COMPREPLY[@]}"
  > EOF
  $ chmod +x test

  $ ./test 'dune '
  build
  cache
  clean
  complete
  coq
  describe
  diagnostics
  exec
  external-lib-deps
  fmt
  format-dune-file
  help
  init
  install
  installed-libraries
  internal
  ocaml
  ocaml-merlin
  printenv
  promote
  promotion
  rpc
  rules
  runtest
  shutdown
  subst
  test
  top
  uninstall
  upgrade
  utop

  $ ./test 'dune b'
  build

  $ ./test 'dune ocaml '
  dump-dot-merlin
  merlin
  ocaml-merlin
  top
  top-module
  utop

  $ ./test 'dune ocaml t'
  top
  top-module

  $ ./test 'dune ocaml bad '
  
  $ ./test 'dune build '
  complete.sh
  test

  $ ./test 'dune build --debug'
  --debug-artifact-substitution
  --debug-backtraces
  --debug-cache
  --debug-dependency-path
  --debug-digests
  --debug-load-dir
  --debug-store-digest-preimage

  $ ./test 'dune describe '
  external-lib-deps
  opam-files
  pp
  workspace

  $ ./test 'dune describe work'
  workspace

  $ ./test 'dune describe -'
  --action-stderr-on-success
  --action-stdout-on-success
  --always-show-command-line
  --auto-promote
  --build-dir
  --build-info
  --cache
  --cache-check-probability
  --cache-storage-mode
  --config-file
  --context
  --debug-artifact-substitution
  --debug-backtraces
  --debug-cache
  --debug-dependency-path
  --debug-digests
  --debug-load-dir
  --debug-store-digest-preimage
  --default-target
  --diff-command
  --disable-promotion
  --display
  --dump-memo-graph
  --dump-memo-graph-format
  --dump-memo-graph-with-timing
  --error-reporting
  --file-watcher
  --for-release-of-packages
  --force
  --format
  --ignore-promoted-rules
  --instrument-with
  --lang
  --no-buffer
  --no-config
  --no-print-directory
  --only-packages
  --passive-watch-mode
  --print-metrics
  --profile
  --promote-install-files
  --react-to-insignificant-changes
  --release
  --require-dune-project-file
  --root
  --sandbox
  --sanitize-for-tests
  --store-orig-source-dir
  --terminal-persistence
  --trace-file
  --verbose
  --wait-for-filesystem-clock
  --watch
  --with-deps
  --with-pps
  --workspace
  -f
  -j
  -p
  -w
  -x

  $ ./test 'dune describe workspace --'
  --action-stderr-on-success
  --action-stdout-on-success
  --always-show-command-line
  --auto-promote
  --build-dir
  --build-info
  --cache
  --cache-check-probability
  --cache-storage-mode
  --config-file
  --context
  --debug-artifact-substitution
  --debug-backtraces
  --debug-cache
  --debug-dependency-path
  --debug-digests
  --debug-load-dir
  --debug-store-digest-preimage
  --default-target
  --diff-command
  --disable-promotion
  --display
  --dump-memo-graph
  --dump-memo-graph-format
  --dump-memo-graph-with-timing
  --error-reporting
  --file-watcher
  --for-release-of-packages
  --force
  --format
  --ignore-promoted-rules
  --instrument-with
  --lang
  --no-buffer
  --no-config
  --no-print-directory
  --only-packages
  --passive-watch-mode
  --print-metrics
  --profile
  --promote-install-files
  --react-to-insignificant-changes
  --release
  --require-dune-project-file
  --root
  --sandbox
  --sanitize-for-tests
  --store-orig-source-dir
  --terminal-persistence
  --trace-file
  --verbose
  --wait-for-filesystem-clock
  --watch
  --with-deps
  --with-pps
  --workspace

  $ ./test 'dune build --sandbox '
  copy
  hardlink
  none
  symlink

  $ ./test 'dune build --sandbox sym'
  symlink

  $ ./test 'dune build --no-config --debug'
  --debug-artifact-substitution
  --debug-backtraces
  --debug-cache
  --debug-dependency-path
  --debug-digests
  --debug-load-dir
  --debug-store-digest-preimage

  $ ./test 'dune build --sandbox symlink --debug'
  --debug-artifact-substitution
  --debug-backtraces
  --debug-cache
  --debug-dependency-path
  --debug-digests
  --debug-load-dir
  --debug-store-digest-preimage

  $ ./test 'dune build --sandbox=symlink --debug'
  --debug-artifact-substitution
  --debug-backtraces
  --debug-cache
  --debug-dependency-path
  --debug-digests
  --debug-load-dir
  --debug-store-digest-preimage

  $ ./test 'dune build --promote-install-files '
  false
  true

  $ ./test 'dune build --promote-install-files --debug'
  --debug-artifact-substitution
  --debug-backtraces
  --debug-cache
  --debug-dependency-path
  --debug-digests
  --debug-load-dir
  --debug-store-digest-preimage

  $ ./test 'dune build --promote-install-files --sandbox symlink --debug'
  --debug-artifact-substitution
  --debug-backtraces
  --debug-cache
  --debug-dependency-path
  --debug-digests
  --debug-load-dir
  --debug-store-digest-preimage

  $ ./test 'dune describe -- --debug'
  

  $ ./test 'dune describe -- --debug work'
  workspace

  $ ./test 'dune describe --doesnotexist '
  

It only completes the positional argument at point:

  $ ./test 'dune complete test several-pos '
  a1
  a2

  $ ./test 'dune complete test several-pos a1 '
  b1
  b2

List completion:

  $ ./test 'dune complete test list '
  aa
  bb

  $ ./test 'dune complete test list a'
  aa

  $ ./test 'dune complete test list aa,'
  aa,aa
  aa,bb

  $ ./test 'dune complete test list aa,b'
  aa,bb

  $ ./test 'dune complete test list aa,bb,'
  aa,bb,aa
  aa,bb,bb

Completion of targets:

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name e))
  > 
  > (rule
  >  (write-file target contents))
  > 
  > (subdir src
  >  (rule
  >   (write-file target contents)))
  > 
  > (subdir src/sub
  >  (rule
  >   (write-file target contents)))
  > EOF

  $ touch e.ml

  $ ./test 'dune build e.e'
  e.exe

  $ ./test 'dune build tar'
  target

  $ ./test 'dune build src/tar'
  src/target

  $ ./test 'dune build /'
  

  $ ./test 'dune build ..'
  
  $ ./test 'dune build @'
  @all
  @default
  @fmt
  @install
  @runtest
  @src/

  $ ./test 'dune build @src/'
  @src/all
  @src/default
  @src/fmt
  @src/install
  @src/runtest
  @src/sub/

  $ ./test 'dune build @src/a'
  @src/all

  $ ./test 'dune build @@'
  @@all
  @@default
  @@fmt
  @@install
  @@runtest
  @@src/
