(run NAME) with a bare literal name and (run %{bin:NAME}) both record
a dep on the build artifact. Before wrv only the bare-literal form
did, since the pform default was where=Install_dir; wrv flips the
pform default to Original_path so both paths agree.

  $ make_mypkg_bin_project hello
  $ cat >dune <<'EOF'
  > (rule
  >  (with-stdout-to out-literal
  >   (run mybin)))
  > (rule
  >  (with-stdout-to out-pform
  >   (run %{bin:mybin})))
  > EOF

(run mybin) records a dep on the build artifact:

  $ dune rules --format=json _build/default/out-literal \
  >   | jq_dune '.[] | ruleDepFilePaths' \
  >   | grep mybin
  "_build/default/src/mybin.exe"

(run %{bin:mybin}) records a dep on the build artifact too:

  $ dune rules --format=json _build/default/out-pform \
  >   | jq_dune '.[] | ruleDepFilePaths' \
  >   | grep mybin
  "_build/default/src/mybin.exe"
