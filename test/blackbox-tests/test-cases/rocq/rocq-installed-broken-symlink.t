Dependencies of implicit Corelib and selected installed Rocq theories.

Set up a fake Rocq installation containing Corelib, one requested installed
library, and an unrelated broken .vo symlink.

  $ export DUNE_CACHE=enabled
  $ unset ROCQPATH FAKE_ROCQ_PREFIX
  $ mkdir -p fake-prefix/bin
  $ mkdir -p fake-prefix/lib/coq/theories/Init
  $ mkdir -p fake-prefix/lib/coq/user-contrib/Good
  $ mkdir -p fake-prefix/lib/coq/user-contrib/Unrelated
  $ touch fake-prefix/lib/coq/theories/Init/Prelude.vo
  $ touch fake-prefix/lib/coq/user-contrib/Good/Good.vo
  $ ln -s missing.vo fake-prefix/lib/coq/user-contrib/Unrelated/Broken.vo

The fake rocq implements only the commands needed to build this test.

  $ cat > fake-prefix/bin/rocq <<'EOF'
  > #!/bin/sh
  > set -eu
  > prefix=${FAKE_ROCQ_PREFIX:-$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)}
  > case "${1-} ${2-}" in
  >   "c --config")
  >     printf 'COQLIB=%s/lib/coq/\n' "$prefix"
  >     printf 'COQ_NATIVE_COMPILER_DEFAULT=no\n'
  >     ;;
  >   "dep "*)
  >     printf 'test.vo:\n'
  >     ;;
  >   "compile "*)
  >     for arg do
  >       case "$arg" in
  >         *.v) source=$arg ;;
  >       esac
  >     done
  >     target=${source%.v}
  >     touch "$target.vo" "$target.glob"
  >     ;;
  >   *)
  >     echo "unexpected invocation: rocq $*" >&2
  >     exit 70
  >     ;;
  > esac
  > EOF
  $ chmod +x fake-prefix/bin/rocq
  $ export PATH=$PWD/fake-prefix/bin:$PATH

The local theory depends only on Good. The unrelated broken symlink must not
prevent the build.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.22)
  > (using rocq 0.12)
  > EOF
  $ mkdir theories
  $ cat > theories/dune <<'EOF'
  > (rocq.theory
  >  (name repro)
  >  (theories Good))
  > EOF
  $ cat > theories/test.v <<'EOF'
  > Check True.
  > EOF

  $ dune build
  File "theories/dune", lines 1-3, characters 0-44:
  1 | (rocq.theory
  2 |  (name repro)
  3 |  (theories Good))
  Error: File unavailable:
  $TESTCASE_ROOT/fake-prefix/lib/coq/user-contrib/Unrelated/Broken.vo
  Broken symbolic link
  [1]

  $ unlink fake-prefix/lib/coq/user-contrib/Unrelated/Broken.vo
  $ dune build

Keep caching enabled for the controls below. The fake compiler deliberately
reports no module imports, so every missing-file error comes from Dune's
installed-theory dependencies, before compilation.

  $ build_result() {
  >   dune build theories/test.vo --display=quiet > build.log 2>&1
  >   result=$?
  >   cat build.log
  >   return "$result"
  > }
  $ ln -s missing.vo fake-prefix/lib/coq/user-contrib/Unrelated/Broken.vo

Disable implicit Corelib. The same requested Good theory now builds despite
the unrelated broken link. Installed-library discovery itself is not enough
to demand every discovered .vo.

  $ cat > theories/dune <<'EOF'
  > (rocq.theory
  >  (name repro)
  >  (stdlib no)
  >  (theories Good))
  > EOF
  $ build_result

A broken file under a descendant of Good is different: selecting the legacy
Good theory includes all of its descendants, even with Corelib disabled and
with no imports reported by rocq dep.

  $ mkdir -p fake-prefix/lib/coq/user-contrib/Good/Extra
  $ mkdir -p fake-prefix/lib/coq/user-contrib/Good/Base
  $ touch fake-prefix/lib/coq/user-contrib/Good/Base/Healthy.vo
  $ ln -s missing.vo fake-prefix/lib/coq/user-contrib/Good/Extra/Broken.vo
  $ build_result
  File "theories/dune", lines 1-4, characters 0-57:
  1 | (rocq.theory
  2 |  (name repro)
  3 |  (stdlib no)
  4 |  (theories Good))
  Error: File unavailable:
  $TESTCASE_ROOT/fake-prefix/lib/coq/user-contrib/Good/Extra/Broken.vo
  Broken symbolic link
  [1]

Selecting Good.Base instead of the parent Good does not include its sibling
Good.Extra. The requested namespace determines the legacy dependency subtree.

  $ cat > theories/dune <<'EOF'
  > (rocq.theory
  >  (name repro)
  >  (stdlib no)
  >  (theories Good.Base))
  > EOF
  $ build_result
  $ cat > theories/dune <<'EOF'
  > (rocq.theory
  >  (name repro)
  >  (stdlib no)
  >  (theories Good))
  > EOF
  $ unlink fake-prefix/lib/coq/user-contrib/Good/Extra/Broken.vo
  $ build_result

An explicitly selected broken legacy theory is also still an error.

  $ cat > theories/dune <<'EOF'
  > (rocq.theory
  >  (name repro)
  >  (stdlib no)
  >  (theories Unrelated))
  > EOF
  $ build_result
  File "theories/dune", lines 1-4, characters 0-62:
  1 | (rocq.theory
  2 |  (name repro)
  3 |  (stdlib no)
  4 |  (theories Unrelated))
  Error: File unavailable:
  $TESTCASE_ROOT/fake-prefix/lib/coq/user-contrib/Unrelated/Broken.vo
  Broken symbolic link
  [1]

A broken file inside Corelib remains a dependency of implicit Corelib.
Remove the user-contrib broken link to isolate this case.

  $ unlink fake-prefix/lib/coq/user-contrib/Unrelated/Broken.vo
  $ ln -s missing.vo fake-prefix/lib/coq/theories/Init/Broken.vo
  $ cat > theories/dune <<'EOF'
  > (rocq.theory
  >  (name repro)
  >  (theories Good))
  > EOF
  $ build_result
  File "theories/dune", lines 1-3, characters 0-44:
  1 | (rocq.theory
  2 |  (name repro)
  3 |  (theories Good))
  Error: File unavailable:
  $TESTCASE_ROOT/fake-prefix/lib/coq/theories/Init/Broken.vo
  Broken symbolic link
  [1]
  $ unlink fake-prefix/lib/coq/theories/Init/Broken.vo
  $ build_result

Corelib also includes .vo files directly in theories, rather than only in
its subdirectories.

  $ ln -s missing.vo fake-prefix/lib/coq/theories/Root.vo
  $ build_result
  File "theories/dune", lines 1-3, characters 0-44:
  1 | (rocq.theory
  2 |  (name repro)
  3 |  (theories Good))
  Error: File unavailable:
  $TESTCASE_ROOT/fake-prefix/lib/coq/theories/Root.vo
  Broken symbolic link
  [1]
  $ unlink fake-prefix/lib/coq/theories/Root.vo
  $ build_result
  $ ln -s missing.vo fake-prefix/lib/coq/user-contrib/Unrelated/Broken.vo

Put the compiler physically in this project's own build directory, using an
absolute PATH entry. Good is still discovered from its configured installation:
physical location alone does not trigger the of_rocq_install guard.

  $ mkdir local-bin
  $ cat > local-bin/dune <<'EOF'
  > (rule
  >  (target rocq)
  >  (deps ../fake-prefix/bin/rocq)
  >  (action
  >   (progn
  >    (copy ../fake-prefix/bin/rocq %{target})
  >    (run chmod +x %{target}))))
  > EOF
  $ dune build local-bin/rocq
  $ cat > theories/dune <<'EOF'
  > (rocq.theory
  >  (name repro)
  >  (stdlib no)
  >  (theories Good))
  > EOF
  $ export outer_bin=$PWD/_build/default/local-bin
  $ export fake_prefix=$PWD/fake-prefix
  $ PATH="$outer_bin:$PATH" FAKE_ROCQ_PREFIX="$fake_prefix" build_result

ROCQPATH can also supply Good. A broken Good descendant still fails, and
removing it recovers the build, while Unrelated remains broken.

  $ export contrib=$fake_prefix/lib/coq/user-contrib
  $ PATH="$outer_bin:$PATH" FAKE_ROCQ_PREFIX="$fake_prefix" \
  >   ROCQPATH="$contrib" build_result
  $ ln -s missing.vo fake-prefix/lib/coq/user-contrib/Good/Extra/Broken.vo
  $ PATH="$outer_bin:$PATH" FAKE_ROCQ_PREFIX="$fake_prefix" \
  >   ROCQPATH="$contrib" build_result
  File "theories/dune", lines 1-4, characters 0-57:
  1 | (rocq.theory
  2 |  (name repro)
  3 |  (stdlib no)
  4 |  (theories Good))
  Error: File unavailable:
  $TESTCASE_ROOT/fake-prefix/lib/coq/user-contrib/Good/Extra/Broken.vo
  Broken symbolic link
  [1]
  $ unlink fake-prefix/lib/coq/user-contrib/Good/Extra/Broken.vo
  $ PATH="$outer_bin:$PATH" FAKE_ROCQ_PREFIX="$fake_prefix" \
  >   ROCQPATH="$contrib" build_result

Now invoke a separate child project with the exact same compiler executable.
The compiler and its configured installation are outside the child's build
directory, even though both are in the parent's _build. The child discovers
Good through of_rocq_install. The original Corelib scan includes Unrelated here.

  $ mkdir -p _build/install/default/lib
  $ cp -R fake-prefix/lib/coq _build/install/default/lib/coq
  $ export outer_prefix=$PWD/_build/install/default
  $ mkdir -p child/theories
  $ cp dune-project child/dune-project
  $ cp theories/test.v child/theories/test.v
  $ cat > child/theories/dune <<'EOF'
  > (rocq.theory
  >  (name repro)
  >  (theories Good))
  > EOF
  $ (cd child && PATH="$outer_bin:$PATH" \
  >   FAKE_ROCQ_PREFIX="$outer_prefix" build_result)
  File "theories/dune", lines 1-3, characters 0-44:
  1 | (rocq.theory
  2 |  (name repro)
  3 |  (theories Good))
  Error: File unavailable:
  $TESTCASE_ROOT/_build/install/default/lib/coq/user-contrib/Unrelated/Broken.vo
  Broken symbolic link
  [1]

Disable Corelib in the child, without setting ROCQPATH. Good is available
from the parent's installation and the unrelated link no longer blocks it.

  $ cat > child/theories/dune <<'EOF'
  > (rocq.theory
  >  (name repro)
  >  (stdlib no)
  >  (theories Good))
  > EOF
  $ (cd child && PATH="$outer_bin:$PATH" \
  >   FAKE_ROCQ_PREFIX="$outer_prefix" build_result)

A lock-dir compiler is instead returned as a Dune-managed build path. Here the
of_rocq_install guard is active. The compiler reports an installation with
Good, but that installed theory is not discovered.

  $ mkdir -p locked/theories locked/dune.lock
  $ cat > locked/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using rocq 0.12)
  > (package (name locked) (depends rocq) (allow_empty))
  > EOF
  $ cat > locked/dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (pkg enabled)
  > EOF
  $ cat > locked/dune.lock/lock.dune <<'EOF'
  > (lang package 0.1)
  > (repositories (complete true))
  > EOF
  $ cat > locked/dune.lock/rocq.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $fake_prefix/bin))
  > (install
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run cp rocq %{prefix}/bin/rocq)))
  > EOF
  $ cp theories/dune locked/theories/dune
  $ cp theories/test.v locked/theories/test.v
  $ (cd locked && FAKE_ROCQ_PREFIX="$fake_prefix" \
  >   dune exec -- rocq c --config)
  COQLIB=$TESTCASE_ROOT/fake-prefix/lib/coq/
  COQ_NATIVE_COMPILER_DEFAULT=no
  $ (cd locked && FAKE_ROCQ_PREFIX="$fake_prefix" build_result)
  File "theories/dune", line 4, characters 11-15:
  4 |  (theories Good))
                 ^^^^
  Theory "Good" has not been found.
  -> required by theory repro in theories/dune:2
  -> required by _build/default/theories/test.vo
  [1]

ROCQPATH scanning remains active when of_rocq_install is skipped. It supplies
Good, and a broken descendant still fails with Corelib disabled. This behavior
is independent of how Corelib's own .vo list is collected.

  $ (cd locked && FAKE_ROCQ_PREFIX="$fake_prefix" \
  >   ROCQPATH="$contrib" build_result)
  $ ln -s missing.vo fake-prefix/lib/coq/user-contrib/Good/Extra/Broken.vo
  $ (cd locked && FAKE_ROCQ_PREFIX="$fake_prefix" \
  >   ROCQPATH="$contrib" build_result)
  File "theories/dune", lines 1-4, characters 0-57:
  1 | (rocq.theory
  2 |  (name repro)
  3 |  (stdlib no)
  4 |  (theories Good))
  Error: File unavailable:
  $TESTCASE_ROOT/fake-prefix/lib/coq/user-contrib/Good/Extra/Broken.vo
  Broken symbolic link
  [1]
  $ unlink fake-prefix/lib/coq/user-contrib/Good/Extra/Broken.vo
  $ (cd locked && FAKE_ROCQ_PREFIX="$fake_prefix" \
  >   ROCQPATH="$contrib" build_result)
