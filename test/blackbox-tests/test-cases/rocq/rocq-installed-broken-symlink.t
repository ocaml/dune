Dune should ignore broken .vo symlinks while scanning installed Rocq theories.

Set up a fake Rocq installation containing Corelib, one requested installed
library, and an unrelated broken .vo symlink.

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
  > prefix=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
  > case "${1-} ${2-}" in
  >   "c --config")
  >     printf 'COQLIB=%s/lib/coq/\n' "$prefix"
  >     printf 'COQ_NATIVE_COMPILER_DEFAULT=no\n'
  >     ;;
  >   "dep -boot"*)
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

  $ unlink fake-prefix/lib/coq/user-contrib/Unrelated/Broken.vo
  $ dune build

