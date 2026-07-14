Landlock policy propagation through compound actions and helper processes.

  $ unset DUNE_CONFIG__LANDLOCK
  $ make_dune_project 3.25
  $ cat >> dune-project <<'EOF'
  > (using action-plugin 0.1)
  > EOF
  $ export OUTSIDE=$PWD/outside
  $ mkdir "$OUTSIDE"
  $ if dune internal with-landlock -- true >/dev/null 2>&1; then
  >   policy=blocked
  > elif test "${CI:-false}" = true && test -d /proc/sys/kernel; then
  >   echo 'Landlock must be available in Linux CI'
  >   exit 1
  > else
  >   policy=wrote
  > fi
  $ cat > probe.sh <<'EOF'
  > if touch "$OUTSIDE/$1" 2>/dev/null; then
  >   echo "$1-wrote" > "$2"
  > else
  >   echo "$1-blocked" > "$2"
  > fi
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (targets progn-1 progn-2)
  >  (deps probe.sh (sandbox always))
  >  (action
  >   (progn
  >    (bash "sh %{dep:probe.sh} progn-1 progn-1")
  >    (bash "sh %{dep:probe.sh} progn-2 progn-2"))))
  > (rule
  >  (targets concurrent-1 concurrent-2)
  >  (deps probe.sh (sandbox always))
  >  (action
  >   (concurrent
  >    (bash "sh %{dep:probe.sh} concurrent-1 concurrent-1")
  >    (bash "sh %{dep:probe.sh} concurrent-2 concurrent-2"))))
  > (rule
  >  (targets pipe pipe-1 pipe-2)
  >  (deps probe.sh (sandbox always))
  >  (action
  >   (with-stdout-to pipe
  >    (pipe-stdout
  >     (bash "sh %{dep:probe.sh} pipe-1 pipe-1; printf 'pipe\\n'")
  >     (bash "sh %{dep:probe.sh} pipe-2 pipe-2; cat")))))
  > (rule
  >  (target plugin)
  >  (deps (sandbox always))
  >  (action
  >   (progn
  >    (dynamic-run action_plugin_helper touch "$OUTSIDE/plugin")
  >    (write-file %{target} plugin-blocked))))
  > EOF

  $ dune build progn-1 progn-2 concurrent-1 concurrent-2 pipe plugin
  $ test "$policy" = blocked || test -e outside/plugin
  $ test "$policy" = blocked || rm -f outside/*
  $ for name in progn-1 progn-2 concurrent-1 concurrent-2 pipe-1 pipe-2; do
  >   test "$(cat _build/default/$name)" = "$name-$policy"
  > done
  $ test "$(cat _build/default/pipe)" = pipe
  $ test "$(cat _build/default/plugin)" = plugin-blocked
  $ test "$policy" = wrote || test ! -e outside/plugin

Cram commands receive the policy too.

  $ cat > policy.t <<EOF
  >   \$ if touch "\$OUTSIDE/cram" 2>/dev/null; then echo wrote; else echo blocked; fi
  >   $policy
  > EOF
  $ cat >> dune <<'EOF'
  > (cram
  >  (deps (sandbox always)))
  > EOF
  $ dune runtest policy.t
  $ test "$policy" = blocked || rm -f outside/*
  $ test "$policy" = wrote || test ! -e outside/cram

The C compiler vendor probe receives the policy.

  $ actual_ocamlc=$(command -v ocamlc)
  $ actual_cc=$(command -v "$(ocamlc -config-var c_compiler)")
  $ cat > ocamlc-wrapper <<EOF
  > #!/bin/sh
  > if test "\$1" = -config; then
  >   "$actual_ocamlc" -config | sed 's|^c_compiler:.*|c_compiler: $PWD/cc-wrapper|'
  > else
  >   exec "$actual_ocamlc" "\$@"
  > fi
  > EOF
  $ cat > cc-wrapper <<EOF
  > #!/bin/sh
  > case " \$* " in
  >   *" -E "*)
  >     touch "$OUTSIDE/cc-vendor" 2>/dev/null || :
  >     echo gcc
  >     ;;
  >   *) exec "$actual_cc" "\$@" ;;
  > esac
  > EOF
  $ chmod +x ocamlc-wrapper cc-wrapper
  $ mkdir -p findlib
  $ external_findlib_path=$(ocamlfind printconf path | tr '\n' ':' | sed 's/:$//')
  $ cat > findlib/findlib.conf <<EOF
  > path="$external_findlib_path"
  > ocamlc="$PWD/ocamlc-wrapper"
  > EOF
  $ cat >> dune <<'EOF'
  > (library
  >  (name foreign)
  >  (foreign_stubs
  >   (language c)
  >   (names foreign)))
  > EOF
  $ echo 'void foreign(void) {}' > foreign.c
  $ OCAMLFIND_CONF=$PWD/findlib/findlib.conf dune build foreign.cma
  $ test "$policy" = blocked || test -e outside/cc-vendor
  $ test "$policy" = blocked || rm -f outside/*
  $ test "$policy" = wrote || test ! -e outside/cc-vendor

External diff commands execute under the action's policy.

  $ echo expected > expected
  $ echo actual > actual
  $ cat >> dune <<'EOF'
  > (rule
  >  (alias diff)
  >  (deps expected actual (sandbox always))
  >  (action (diff expected actual)))
  > EOF
  $ cat > diff.sh <<'EOF'
  > if touch "$OUTSIDE/diff" 2>/dev/null; then echo wrote; else echo blocked; fi
  > exit 1
  > EOF
  $ dune build @diff --diff-command "sh $PWD/diff.sh" >diff.output 2>&1 || :
  $ test "$policy" = blocked || rm -f outside/*
  $ grep -qx "$policy" diff.output
  $ test "$policy" = wrote || test ! -e outside/diff

The external diff used to report a timed-out cram test also receives the policy.

  $ cat >> dune <<'EOF'
  > (cram
  >  (applies_to timeout)
  >  (deps (sandbox always))
  >  (timeout 0.05))
  > EOF
  $ cat > timeout.t <<'EOF'
  >   $ sleep 1
  > EOF
  $ rm -f outside/diff
  $ dune runtest timeout.t --diff-command "sh $PWD/diff.sh" >timeout.output 2>&1 || :
  $ test "$policy" = blocked || rm -f outside/*
  $ grep -qx "$policy" timeout.output
  $ test "$policy" = wrote || test ! -e outside/diff
