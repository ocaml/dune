A binary registered via [(env (_ (binaries ...)))] becomes a [Resolved] entry
in [local_bins] (Artifacts.add_binaries) with NO owning package. So,
env-registered binaries are exempt from narrowing entirely.

  $ make_lockdir

A workspace executable exposed under a different name via [(env (binaries ...))]:

  $ cat >mytool.ml <<'EOF'
  > let () = print_endline "from env binary"
  > EOF
  $ cat >dune <<'EOF'
  > (executable (name mytool))
  > (env (_ (binaries (mytool.exe as mybin))))
  > (rule
  >  (with-stdout-to mybin-avail (echo %{bin-available:mybin})))
  > (rule
  >  (enabled_if %{bin-available:mybin})
  >  (action (with-stdout-to mybin-out (run %{bin:mybin}))))
  > EOF

  $ make_dune_project 3.25
  $ cat >> dune-project << 'EOF'
  > (package (name mypkg) (allow_empty) (dir .))
  > EOF

[mybin] resolves despite no declared deps, because env-registered binaries are
not narrowed:

  $ dune build @all
  $ cat _build/default/mybin-avail
  true
  $ cat _build/default/mybin-out
  from env binary

An env binary colliding with a lock-directory binary
-----------------------------------------------------

Env binaries take precedence over package binaries during artifact resolution,
and they keep that precedence on the action's [PATH]. Create a locked package
that installs another [mybin], plus a distinct binary used to force the package
to be built before the action runs:

  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > mybin <<'EOI'
  >           "\| #!/bin/sh
  >           "\| echo from lockdir
  >           "\| EOI
  >   )
  >   (system "\| cat > force-provider <<'EOI'
  >           "\| #!/bin/sh
  >           "\| echo force provider
  >           "\| EOI
  >   )
  >   (system "chmod +x mybin force-provider")
  >   (system "echo 'bin: [ \"mybin\" \"force-provider\" ]' > provider.install")
  >  ))
  > EOF

  $ make_dune_project 3.25
  $ cat >>dune-project <<'EOF'
  > (package (name mypkg) (allow_empty) (dir .) (depends provider))
  > EOF
  $ cat >>dune <<'EOF'
  > (rule
  >  (deps %{bin:force-provider})
  >  (action (with-stdout-to mybin-from-path (system mybin))))
  > (rule
  >  (deps (package provider))
  >  (action (with-stdout-to mybin-from-pkg-dep (system mybin))))
  > EOF

The pform still selects the env binding:

  $ dune clean
  $ dune build mybin-out mybin-from-path mybin-from-pkg-dep
  $ cat _build/default/mybin-out
  from env binary

A bare-name lookup preserves it too. Here [%{bin:force-provider}] stages a
different binary, so nothing competes with the env binding on [PATH]:

  $ cat _build/default/mybin-from-path
  from env binary

[(deps (package provider))] puts the package's bin directory on the action's
[PATH], so a second [mybin] is reachable there. Both directories end up on
[PATH], but the one holding the env binding is placed first:

  $ cat _build/default/mybin-from-pkg-dep
  from env binary

Removing only the env binding leaves the lock directory's [mybin] as the sole
candidate, so every lookup now finds it. Without this, the assertions above
would also hold if [provider]'s [mybin] were never visible in the first place:

  $ cat >dune <<'EOF'
  > (executable (name mytool))
  > (rule
  >  (with-stdout-to mybin-avail (echo %{bin-available:mybin})))
  > (rule
  >  (enabled_if %{bin-available:mybin})
  >  (action (with-stdout-to mybin-out (run %{bin:mybin}))))
  > (rule
  >  (deps %{bin:force-provider})
  >  (action (with-stdout-to mybin-from-path (system mybin))))
  > (rule
  >  (deps (package provider))
  >  (action (with-stdout-to mybin-from-pkg-dep (system mybin))))
  > EOF

  $ dune clean
  $ dune build mybin-avail mybin-out mybin-from-path mybin-from-pkg-dep
  $ cat _build/default/mybin-avail
  true
  $ cat _build/default/mybin-out
  from lockdir
  $ cat _build/default/mybin-from-path
  from lockdir
  $ cat _build/default/mybin-from-pkg-dep
  from lockdir
