Demonstrate the shell field in the cram stanza

  $ make_dune_project 3.22

  $ printShell() {
  > dune trace cat | jq_dune 'processes | select(.args.categories | index("cram")) | .args | .prog | split("/") | last'
  > }

  $ cat >foo.t <<'EOF'
  >   $ echo foo
  > EOF

  $ dune runtest foo.t
  File "foo.t", line 1, characters 0-0:
  --- foo.t
  +++ foo.t.corrected
  @@ -1 +1,2 @@
     $ echo foo
  +  foo
  [1]
  $ printShell
  "sh"

  $ cat > dune <<EOF
  > (cram
  >  (shell bash))
  > EOF

  $ dune runtest foo.t
  File "foo.t", line 1, characters 0-0:
  --- foo.t
  +++ foo.t.corrected
  @@ -1 +1,2 @@
     $ echo foo
  +  foo
  [1]
  $ printShell
  "bash"

  $ cat >options.t <<'EOF'
  >   $ set -o | grep -e pipefail -e nounset -e errexit
  >   errexit        	off
  >   nounset        	on
  >   pipefail       	on
  > EOF

  $ dune runtest options.t

  $ cat >code.t <<'EOF'
  >   $ function foo() { return 1; }
  >   $ foo
  >   [1]
  > EOF

  $ dune runtest code.t

  $ cat >exit1.t <<EOF
  >   $ false
  >   [1]
  > EOF

  $ dune runtest exit1.t

Undefined Variables:

  $ cat >undefined.t <<'EOF'
  >   $ echo $foo
  >   ***** UNREACHABLE *****
  > EOF

  $ dune runtest undefined.t --diff-command -
  File "undefined.t", line 1, characters 0-0:
  Error: Files _build/default/undefined.t and
  _build/default/undefined.t.corrected differ.
  [1]

  $ dune promotion show undefined.t 2>&1 | head -n 2 | dune_cmd subst ".*line 1:" "line 1:"
    $ echo $foo
  line 1: foo: unbound variable

If the selected shell is missing, report that before attempting the test. Keep
an OCaml toolchain on PATH so context initialization can still succeed.

  $ dune_bin=$(command -v dune)
  $ export OCAMLLIB=$(ocamlc -where)
  $ mkdir toolchain
  $ for tool in ocamlc ocamldep ocamlopt; do
  >   ln -s "$(command -v "$tool")" "toolchain/$tool"
  > done
  $ cat > dune <<'EOF'
  > (cram (shell sh))
  > EOF
  $ PATH="$PWD/toolchain" "$dune_bin" runtest exit1.t
  Error: CRAM test aborted, "sh" can not be found in PATH
  -> required by _build/default/.cram.exit1.t/cram.out
  -> required by alias exit1
  [1]
