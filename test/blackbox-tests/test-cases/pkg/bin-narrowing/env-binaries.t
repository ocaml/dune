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
