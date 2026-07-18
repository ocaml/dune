Repository-wide lint rules for unstable test output. The output of each search
is an explicit baseline, not an invitation to add entries. New output should
normally be removed rather than accepted with `dune promote`.

OCaml exception backtraces and assertion failures can contain source locations.
These locations change as the implementation moves. Tests should censor them
unless the location itself is essential to the behavior under test.

  $ cd "$INSIDE_DUNE"
  $ rg --hidden --no-heading --with-filename --no-line-number \
  >   -g '!.jj/**' -g '!**/.cram.*' \
  >   -g '!test/blackbox-tests/test-cases/repository-lint.t' \
  >   '(Fatal error: exception (File "[^"]+", line [0-9]+, characters [0-9]+-[0-9]+|Assert_failure\("[^"]+",[[:space:]]*[0-9]+,[[:space:]]*[0-9]+\))|((Raised|Re-raised) at |Raised by primitive operation at |Called from ).*in file "[^"]+", line [0-9]+|^[[:space:]]+"[^"]+", line [0-9]+, characters [0-9]+-[0-9]+)' \
  > | LC_ALL=C sort
  doc/howto/use-multiple-build-contexts.md:Fatal error: exception Assert_failure("tsan_check.ml", 19, 2)
  test/blackbox-tests/test-cases/inline-tests/generate-runner-sandboxing.t:  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  test/blackbox-tests/test-cases/inline-tests/generate-runner-sandboxing.t:  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  test/blackbox-tests/test-cases/inline-tests/simple.t:  Fatal error: exception File ".foo_simple.inline-tests/main.ml-gen", line 1, characters 40-46: Assertion failed
  test/blackbox-tests/test-cases/inline-tests/simple.t:  Fatal error: exception File ".foo_simple.inline-tests/main.ml-gen", line 1, characters 40-46: Assertion failed
  test/blackbox-tests/test-cases/inline-tests/simple.t:  Fatal error: exception File ".foo_simple.inline-tests/main.ml-gen", line 1, characters 40-46: Assertion failed
  test/blackbox-tests/test-cases/inline-tests/simple.t:  Fatal error: exception File ".foo_simple.inline-tests/main.ml-gen", line 1, characters 40-46: Assertion failed
  test/blackbox-tests/test-cases/jsoo/runtest-cmd-inline-tests.t:  Fatal error: exception File "jsoo_lib.ml", line 5, characters 8-14: Assertion failed
  test/blackbox-tests/test-cases/runtest/runtest-cmd-inline-tests.t:  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  test/blackbox-tests/test-cases/runtest/runtest-cmd-inline-tests.t:  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  test/blackbox-tests/test-cases/runtest/runtest-cmd-inline-tests.t:  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed
  test/blackbox-tests/test-cases/runtest/runtest-cmd-inline-tests.t:  Fatal error: exception File ".mylib.inline-tests/main.ml-gen", line 2, characters 40-46: Assertion failed

Short display on `dune build` and `dune exec` is intended for humans and
exposes unstable details of build scheduling and command rendering. Tests
should inspect traces or rules instead. Do not add another use unless short
display itself is under test.

  $ rg --hidden --no-heading --with-filename --no-line-number \
  >   -g '!.jj/**' -g '!**/.cram.*' \
  >   -g '!test/blackbox-tests/test-cases/repository-lint.t' \
  >   'dune (build|exec).*--display(?:[ =])short' \
  > | LC_ALL=C sort
  .github/workflows/multi-repo-build.yml:        default: 'dune build --display short'
  .github/workflows/multi-repo-build.yml:        description: 'Main command to run (default: "dune build --display short")'
  test/blackbox-tests/test-cases/actions/with-exit-codes.t:  $ dune build --display=short --root . @a
  test/blackbox-tests/test-cases/actions/with-exit-codes.t:  $ dune build --display=short --root . @b
  test/blackbox-tests/test-cases/actions/with-exit-codes.t:  $ dune build --display=short --root . @c
  test/blackbox-tests/test-cases/actions/with-exit-codes.t:  $ dune build --display=short --root . @d
  test/blackbox-tests/test-cases/actions/with-exit-codes.t:  $ dune build --display=short --root . @e
  test/blackbox-tests/test-cases/actions/with-nested-exit-codes.t:  $ dune build --display=short --root . @f
  test/blackbox-tests/test-cases/actions/with-nested-exit-codes.t:  $ dune build --display=short --root . @f2
  test/blackbox-tests/test-cases/actions/with-nested-exit-codes.t:  $ dune build --display=short --root . @f3
  test/blackbox-tests/test-cases/actions/with-nested-exit-codes.t:  $ dune build --display=short --root . @f4
  test/blackbox-tests/test-cases/actions/with-nested-exit-codes.t:  $ dune build --display=short --root . @f5
  test/blackbox-tests/test-cases/actions/with-nested-exit-codes.t:  $ dune build --display=short --root . @g
  test/blackbox-tests/test-cases/alias/all-alias/install-alias.t/run.t:  $ dune build --display short @all
  test/blackbox-tests/test-cases/alias/all-alias/private-lib.t/run.t:  $ dune build --display short @all 2>&1 | grep bar.cma
  test/blackbox-tests/test-cases/cinaps/custom-alias.t:  $ dune build @foo --display short 2>&1 | grep alias
  test/blackbox-tests/test-cases/cram/git-diff-fail.t:  $ bash -c 'set -o pipefail; dune build --always-show-command-line --root=. --diff-command="exit 1; echo" --display=short @runtest 2>&1 | grep -v "(cd"' 
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short --always-show-command-line
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short --always-show-command-line
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short --always-show-command-line
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short --always-show-command-line
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short --always-show-command-line
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short --always-show-command-line
  test/blackbox-tests/test-cases/display.t:  $ dune clean; dune build --display short --always-show-command-line
  test/blackbox-tests/test-cases/inline-tests/parallel.t/run.t:  $ dune build --display short @test1/runtest 2>&1 | grep alias
  test/blackbox-tests/test-cases/inline-tests/parallel.t/run.t:  $ dune build --display short @test2/runtest 2>&1 | grep alias
  test/blackbox-tests/test-cases/inline-tests/parallel.t/run.t:  $ dune build --display short @test3/runtest 2>&1 | grep alias
  test/blackbox-tests/test-cases/jsoo/without_implem.t/run.t:  $ dune build --display=short main.bc.js
  test/blackbox-tests/test-cases/jsoo/without_implem.t/run.t:  $ dune build --display=short main.bc.js
  test/blackbox-tests/test-cases/lib-available.t/run.t:  $ dune build @runtest --display short --debug-dependency-path 2>&1 | sed "s/ cmd /  sh /"
  test/blackbox-tests/test-cases/pkg/autolock-detects-changes.t:  $ dune exec --display short bar 2>&1 | grep "Building"
  test/blackbox-tests/test-cases/pkg/autolock-detects-changes.t:  $ dune exec --display short bar 2>&1 | grep "Building"
  test/blackbox-tests/test-cases/pkg/autolock-detects-changes.t:  $ dune exec --display short bar 2>&1 | grep "Building" || echo "no rebuilds"
  test/blackbox-tests/test-cases/pkg/build-progress.t:  $ dune build @pkg-install --display short
  test/blackbox-tests/test-cases/pkg/pkg-lock-then-autolock.t:  $ dune exec --display short bar 2>&1 | grep "Building"
  test/blackbox-tests/test-cases/pkg/pkg-lock-then-autolock.t:  $ dune exec --display short bar 2>&1 | grep "Building" || echo "no rebuilds"
  test/blackbox-tests/test-cases/rocq/base-unsound.t/run.t:  $ dune build --display short --profile unsound --debug-dependency-path @all
  test/blackbox-tests/test-cases/rocq/base.t/run.t:  $ dune build --display short --debug-dependency-path @all --always-show-command-line
  test/blackbox-tests/test-cases/rocq/compose-installed-corelib.t/run.t:  $ dune build test.vo --display=short --always-show-command-line
  test/blackbox-tests/test-cases/rocq/compose-installed-rebuild.t/run.t:  $ dune build --root A --display=short
  test/blackbox-tests/test-cases/rocq/coqdep-on-rebuild.t/run.t:  $ dune build --display short --debug-dependency-path
  test/blackbox-tests/test-cases/rocq/coqdep-on-rebuild.t/run.t:  $ dune build --display short --debug-dependency-path
  test/blackbox-tests/test-cases/rocq/no-rebuild-on-dep-change.t:  $ dune build --display=short
  test/blackbox-tests/test-cases/rocq/no-stdlib.t/run.t:  $ dune build --display=short bar.vo
  test/blackbox-tests/test-cases/rocq/no-stdlib.t/run.t:  $ dune build --display=short foo.vo
  test/blackbox-tests/test-cases/rocq/per_file_flags.t/run.t:  $ dune build --display short @all
  test/blackbox-tests/test-cases/rocq/public-dep-on-private.t/run.t:  $ dune build --display short --debug-dependency-path
  test/blackbox-tests/test-cases/rocq/rec-module.t/run.t:  $ dune build --display short --debug-dependency-path @all
  test/blackbox-tests/test-cases/rocq/rocq-native/base-unsound.t/run.t:  $ dune build --display short --profile unsound --debug-dependency-path @all
  test/blackbox-tests/test-cases/rocq/rocq-native/base.t/run.t:  $ dune build --display short --debug-dependency-path @all --always-show-command-line
  test/blackbox-tests/test-cases/rocq/rocq-native/compose-installed-corelib.t/run.t:  $ dune build test.vo --display=short --always-show-command-line
  test/blackbox-tests/test-cases/rocq/rocq-native/compose-installed-rebuild.t/run.t:  $ dune build --root A --display=short
  test/blackbox-tests/test-cases/rocq/rocq-native/coqdep-on-rebuild.t/run.t:  $ dune build --display short --debug-dependency-path
  test/blackbox-tests/test-cases/rocq/rocq-native/coqdep-on-rebuild.t/run.t:  $ dune build --display short --debug-dependency-path
  test/blackbox-tests/test-cases/rocq/rocq-native/native-compose.t/run.t:  $ dune build --profile=release --display short --debug-dependency-path @all
  test/blackbox-tests/test-cases/rocq/rocq-native/native-single.t/run.t:  $ dune build --profile=release --display short --debug-dependency-path @all
  test/blackbox-tests/test-cases/rocq/rocq-native/no-stdlib.t/run.t:  $ dune build --display=short bar.vo
  test/blackbox-tests/test-cases/rocq/rocq-native/no-stdlib.t/run.t:  $ dune build --display=short foo.vo
  test/blackbox-tests/test-cases/rocq/rocq-native/per_file_flags.t/run.t:  $ dune build --display short @all
  test/blackbox-tests/test-cases/rocq/rocq-native/public-dep-on-private.t/run.t:  $ dune build --display short --debug-dependency-path
  test/blackbox-tests/test-cases/rocq/rocq-native/rec-module.t/run.t:  $ dune build --display short --debug-dependency-path @all
  test/blackbox-tests/test-cases/rocq/vos-build.t/run.t:  $ dune build --display short --debug-dependency-path @all
  test/blackbox-tests/test-cases/rocq/vos-build.t/run.t:  $ dune build --display short --debug-dependency-path @all
  test/blackbox-tests/test-cases/rocq/vos-build.t/run.t:  $ dune build --display short bar.vos
  test/blackbox-tests/test-cases/rocq/vos-build.t/run.t:  $ dune build --display short foo.vos
  test/blackbox-tests/test-cases/select-field/select-validation.t:  $ dune build --display=short
