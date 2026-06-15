[(depopts ...)] declares an optional dependency: the package may or may not be
present, and [%{bin-available:...}] is how a rule asks whether it is. The two
are meant to be used together.

The narrowing walks the owning package's [(depends ...)] and [(depopts ...)]
fields. Rule guarded by [%{bin-available:...}] should run correctly when the
package providing the binary is listed in either of the fields.

  $ make_lockdir

  $ make_lockpkg optional-tool <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > opt-tool <<'EOI'
  >           "\| #!/bin/sh
  >           "\| echo from optional-tool
  >           "\| EOI
  >   )
  >   (system "chmod +x opt-tool")
  >   (system "echo 'bin: [ \"opt-tool\" ]' > optional-tool.install")
  >  ))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (with-stdout-to avail (echo %{bin-available:opt-tool})))
  > (rule
  >  (enabled_if %{bin-available:opt-tool})
  >  (action (with-stdout-to out (run %{bin:opt-tool}))))
  > EOF

Declared as a hard dependency, the binary is available and the guarded rule
runs:

  $ make_dune_project 3.25
  $ cat >>dune-project <<'EOF'
  > (package (name mypkg) (allow_empty) (dir .) (depends optional-tool))
  > EOF

  $ dune build @all
  $ cat _build/default/avail
  true
  $ cat _build/default/out
  from optional-tool

Moving the same package to [(depopts ...)], with the lock directory unchanged,
makes no difference:

  $ make_dune_project 3.25
  $ cat >>dune-project <<'EOF'
  > (package (name mypkg) (allow_empty) (dir .) (depopts optional-tool))
  > EOF

  $ dune clean
  $ dune build @all
  $ cat _build/default/avail
  true
  $ cat _build/default/out
  from optional-tool
