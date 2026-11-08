Strict package checks must account for dynamically discovered dependencies.
Both actions below read the same build-tree input owned by the provider package,
without using install staging paths or package-files aliases.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.24)
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
  >   (with-stdout-to output (dynamic-run ./foo.exe read input))))
  > EOF

This is a bug: DAP does not report the missing provider dependency, either after
executing the action or when inspecting the workspace-cached result.

  $ dune build consumer.install > dynamic-fresh.output 2>&1
  $ dune build @install > dynamic-cached.output 2>&1

A regular action declaring the same input reports the missing dependency in
both cases. Use a separate build directory so its first build is also fresh.

  $ cat > dune <<'EOF'
  > (install (package provider) (section share) (files input))
  > (install (package consumer) (section share) (files output))
  > (rule
  >  (target output)
  >  (action (with-stdout-to output (run cat %{dep:input}))))
  > EOF
  $ dune build --build-dir=_build-static consumer.install > static-fresh.output 2>&1
  [1]
  $ grep -E '^(Error:|- provider)' static-fresh.output
  Error: Package consumer is missing the following package dependencies
  - provider
  $ dune build --build-dir=_build-static @install > static-cached.output 2>&1
  [1]
  $ grep -E '^(Error:|- provider)' static-cached.output
  Error: Package consumer is missing the following package dependencies
  - provider
