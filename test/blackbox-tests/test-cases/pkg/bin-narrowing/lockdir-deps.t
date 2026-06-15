In Lock-kind contexts, [%{bin:X}] and [%{bin-available:X}] resolution would
previously fall through to [Pkg_rules.which] via [Context.which].
[Context.which] now calls [Pkg_rules.which_narrowed_to_packages] which takes a
narrowed list of packages to consider for looking up binaries provided by
packages in the lockdir.

  $ make_lockdir

A lockdir package [provider] that installs a binary [mybin]:

  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > mybin <<'EOI'
  >           "\| #!/usr/bin/env bash
  >           "\| echo from provider
  >           "\| EOI
  >   )
  >  (system "chmod +x mybin")
  >  (system "echo 'bin: [ \"mybin\" ]' > provider.install")
  >  ))
  > EOF

Another lockdir package [check-env] that installs a binary [check-env]:

  $ make_lockpkg check-env <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > check-env <<'EOI'
  >           "\| #!/usr/bin/env bash
  >           "\| echo from check-env
  >           "\| EOI
  >   )
  >  (system "chmod +x check-env")
  >  (system "echo 'bin: [ \"check-env\" ]' > check-env.install")
  >  ))
  > EOF

With the default full-lockdir lookup, [check-env] is found and the rule is
enabled & [mybin] is found and executed, without the need for explicitly
declaring any package dependencies:

  $ make_dune_project 3.24

  $ cat >dune <<'EOF'
  > (rule
  >  (alias checkenv)
  >  (enabled_if %{bin-available:check-env})
  >  (action
  >    (with-stdout-to path-output (bash "echo $PATH"))))
  > (rule
  >  (alias test)
  >  (enabled_if %{bin-available:check-env})
  >  (action
  >    (with-stdout-to mybin-output (run %{bin:mybin}))))
  > (rule
  >  (alias test-sys)
  >  (enabled_if %{bin-available:check-env})
  >  (deps (package provider))
  >  ; Check that the binary can be found on PATH
  >  (action
  >    (with-stdout-to system-mybin-output (system mybin))))
  > EOF

  $ dune build @all

  $ cat _build/default/mybin-output
  from provider

The rule depends on the binary from the provider lockdir package:

  $ dune rules --format=json @test | jq_dune '.[] | ruleDepFilePaths' | censor
  "_build/_private/default/.pkg/provider.0.0.1-$DIGEST/target/bin/mybin"

All the packages' bin layouts are added to $PATH:

  $ cat _build/default/system-mybin-output
  from provider

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST1/target/bin
  $PWD/_build/_private/default/.pkg/check-env.0.0.1-$DIGEST2/target/bin

With a package defined in the project, *without a dir field*, the behavior is
the same.

  $ make_dune_project 3.24
  $ cat >> dune-project << 'EOF'
  > (package
  >   (allow_empty)
  >   (name my-bin-pkg))
  > EOF

  $ dune clean
  $ dune build @all

  $ cat _build/default/mybin-output
  from provider

All the packages' bin layouts are added to $PATH:

  $ cat _build/default/system-mybin-output
  from provider

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST1/target/bin
  $PWD/_build/_private/default/.pkg/check-env.0.0.1-$DIGEST2/target/bin


With a package defined in the project, *with a dir field, but no dependencies*,
the program mybin is not found in PATH or via the bin pform, since the rule for
building it gets disabled:

  $ make_dune_project 3.24
  $ cat >> dune-project << 'EOF'
  > (package
  >   (allow_empty)
  >   (name my-bin-pkg)
  >   (dir .))
  > EOF

  $ dune clean
  $ dune build @all

  $ cat _build/default/mybin-output
  cat: _build/default/mybin-output: No such file or directory
  [1]

The rules adding a dependency on the lock dir and for building [mybin] are
disabled.

  $ cat _build/default/system-mybin-output
  cat: _build/default/system-mybin-output: No such file or directory
  [1]

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  cat: _build/default/path-output: No such file or directory
  

With a package defined in the project, *with a dir field, and explicit depends
on only [check-env]*, the program mybin is still not found via the bin pform,
but can be found on the PATH.

  $ make_dune_project 3.24
  $ cat >> dune-project << 'EOF'
  > (package
  >   (allow_empty)
  >   (name my-bin-pkg)
  >   (dir .)
  >   (depends check-env))
  > EOF

  $ dune clean
  $ dune build @all
  File "dune", line 10, characters 37-49:
  10 |    (with-stdout-to mybin-output (run %{bin:mybin}))))
                                            ^^^^^^^^^^^^
  Error: Program mybin not found in the tree or in PATH
   (context: default)
  Hint: "mybin" is not provided by any dependency of this directory's package.
  Add a dependency on the package that provides it.
  [1]

  $ cat _build/default/mybin-output
  cat: _build/default/mybin-output: No such file or directory
  [1]

Currently, the filtering is only happening when running 'which' and not when
setting up the context's env. So, all the packages' bin layouts are added to
$PATH:

  $ cat _build/default/system-mybin-output
  from provider

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST1/target/bin
  $PWD/_build/_private/default/.pkg/check-env.0.0.1-$DIGEST2/target/bin

With a package defined in the project, *with a dir field, and explicit depends
on only [provider]*, the rules for building [mybin] are disabled since the
[check-env] binary is not available.

  $ make_dune_project 3.24
  $ cat >> dune-project << 'EOF'
  > (package
  >   (allow_empty)
  >   (name my-bin-pkg)
  >   (dir .)
  >   (depends provider))
  > EOF

  $ dune clean
  $ dune build @all

  $ cat _build/default/mybin-output
  cat: _build/default/mybin-output: No such file or directory
  [1]

  $ cat _build/default/system-mybin-output
  cat: _build/default/system-mybin-output: No such file or directory
  [1]

The path output is not generated since the check-env package is missing from (depends ...):

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  cat: _build/default/path-output: No such file or directory
  

With a package defined in the project, *with a dir field, and explicit depends
on both [check-env] and [provider]*, mybin is found both on the PATH and via
the bin pform.

  $ make_dune_project 3.24
  $ cat >> dune-project << 'EOF'
  > (package
  >   (allow_empty)
  >   (name my-bin-pkg)
  >   (dir .)
  >   (depends check-env provider))
  > EOF

  $ dune clean
  $ dune build @all

  $ cat _build/default/mybin-output
  from provider

The bin layouts for all the packages that we depend on, are added to $PATH:

  $ cat _build/default/system-mybin-output
  from provider

  $ env_added "$(cat _build/default/path-output)" "$PATH" | censor
  $PWD/_build/_private/default/.pkg/provider.0.0.1-$DIGEST1/target/bin
  $PWD/_build/_private/default/.pkg/check-env.0.0.1-$DIGEST2/target/bin
