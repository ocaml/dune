Package dependency inference currently ignores dynamically discovered inputs,
including when it inspects a rule restored from the workspace cache.

  $ cat > dune-project <<'EOF'
  > (lang dune 2.3)
  > (using action-plugin 0.1)
  > (name sample)
  > (strict_package_deps)
  > (package (name consumer))
  > (package (name provider))
  > EOF
  $ cp bin/foo.exe .
  $ printf input > input
  $ cat > dune <<'EOF'
  > (install (package provider) (section share) (files input))
  > (install (package consumer) (section share) (files output))
  > (rule
  >  (target output)
  >  (action
  >   (with-stdout-to output
  >    (dynamic-run ./foo.exe read ../install/default/share/provider/input))))
  > EOF
  $ dune build consumer.install > fresh.output 2>&1; echo $?
  0
  $ dune build @install > cached.output 2>&1; echo $?
  0

Without strict package checks, the package-files alias should still include the
inferred dependency's other installed files.

  $ mkdir ordinary
  $ cp bin/foo.exe ordinary/
  $ cd ordinary
  $ cat > dune-project <<'EOF'
  > (lang dune 3.24)
  > (using action-plugin 0.1)
  > (name sample)
  > (package (name consumer))
  > (package (name provider))
  > EOF
  $ printf input > input
  $ printf extra > extra
  $ cat > dune <<'EOF'
  > (install (package provider) (section share) (files input extra))
  > (install (package consumer) (section share) (files output))
  > (rule
  >  (target output)
  >  (action
  >   (with-stdout-to output
  >    (dynamic-run ./foo.exe read ../install/default/share/provider/input))))
  > EOF
  $ dune build @.consumer-files > dynamic.output 2>&1
  $ test -f _build/install/default/share/provider/input
  $ test -f _build/install/default/share/provider/extra
  [1]

A static read of the same installed file does infer the package dependency.

  $ cat > dune <<'EOF'
  > (install (package provider) (section share) (files input extra))
  > (install (package consumer) (section share) (files output))
  > (rule
  >  (target output)
  >  (action (copy %{pkg:provider:share:input} output)))
  > EOF
  $ dune build @.consumer-files --build-dir _build-static
  $ test -f _build-static/install/default/share/provider/extra
