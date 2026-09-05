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

Env binaries also take precedence over package binaries during artifact
resolution. They should have the same precedence on the action's [PATH].
Create a locked package that installs another [mybin], plus a distinct binary
used to force the package to be built before the action runs:

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
  > EOF

The pform still selects the env binding:

  $ dune clean
  $ dune build mybin-out mybin-from-path
  $ cat _build/default/mybin-out
  from env binary

Bare-name lookups also preserve this precedence order:

  $ cat _build/default/mybin-from-path
  from env binary
