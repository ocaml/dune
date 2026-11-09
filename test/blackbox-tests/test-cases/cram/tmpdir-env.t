Observe temporary-directory defaults and overrides in regular and Cram actions.
Both TMPDIR and TEMP are set so the probe observes the platform's variable.
The cram action allows the same setenv wrapper to be tested in both contexts.

  $ make_dune_project 3.25
  $ cat > probe.ml <<'EOF'
  > let () =
  >   let var = if Sys.win32 then "TEMP" else "TMPDIR" in
  >   print_endline (Sys.getenv var)
  > EOF
  $ cat > dune <<'EOF'
  > (executable (name probe))
  > (alias
  >  (name observe)
  >  (deps
  >   default/regular-tmpdir default/cram-tmpdir
  >   env/regular-tmpdir env/cram-tmpdir
  >   setenv/regular-tmpdir setenv/cram-tmpdir))
  > EOF
  $ for dir in default env setenv; do
  >   mkdir "$dir"
  >   cat > "$dir/observe.t" <<'EOF'
  >   $ ../probe.exe > cram-tmpdir
  > EOF
  > done
  $ cat > default/dune <<'EOF'
  > (rule
  >  (target regular-tmpdir)
  >  (deps (sandbox always))
  >  (action (with-stdout-to %{target} (run ../probe.exe))))
  > (rule
  >  (target cram-tmpdir)
  >  (deps ../probe.exe (sandbox always))
  >  (action (cram observe.t)))
  > EOF
  $ cp default/dune env/dune
  $ cat >> env/dune <<'EOF'
  > (env
  >  (_
  >   (env-vars
  >    (TMPDIR env-stanza)
  >    (TEMP env-stanza))))
  > EOF
  $ cat > setenv/dune <<'EOF'
  > (rule
  >  (target regular-tmpdir)
  >  (deps (sandbox always))
  >  (action
  >   (setenv TMPDIR action-override
  >    (setenv TEMP action-override
  >     (with-stdout-to %{target} (run ../probe.exe))))))
  > (rule
  >  (target cram-tmpdir)
  >  (deps ../probe.exe (sandbox always))
  >  (action
  >   (setenv TMPDIR action-override
  >    (setenv TEMP action-override (cram observe.t)))))
  > EOF

An inherited directory is replaced by Dune's managed directory. Currently both
forms of explicit override are also replaced, in regular and Cram actions alike.
Normalize generated directory names, but leave override values unchanged.

  $ inherited="$TMPDIR/tmpdir-env-matrix"
  $ mkdir "$inherited"
  $ TMPDIR="$inherited" TEMP="$inherited" dune build @observe
  $ for mode in default env setenv; do
  >   for kind in regular cram; do
  >     printf '%s %s: ' "$mode" "$kind"
  >     sed -E \
  >       -e 's@\\@/@g' \
  >       -e 's@^.*/dune_[0-9a-f]+_actions@$DUNE_ACTION_TMPDIR@' \
  >       -e 's@dune_cram_[0-9a-f]+_@dune_cram_TEMP_@g' \
  >       "_build/default/$mode/$kind-tmpdir"
  >   done
  > done
  default regular: $DUNE_ACTION_TMPDIR
  default cram: $DUNE_ACTION_TMPDIR
  env regular: $DUNE_ACTION_TMPDIR
  env cram: $DUNE_ACTION_TMPDIR
  setenv regular: $DUNE_ACTION_TMPDIR
  setenv cram: $DUNE_ACTION_TMPDIR

A second invocation must reuse both regular and Cram results rather than having
new temporary paths invalidate the rules. Compiler configuration probes may run.

  $ TMPDIR="$inherited" TEMP="$inherited" dune build @observe
  $ dune trace cat | jq_dune '
  >   processes
  >   | select((.args.categories | index("cram"))
  >            or (.args.prog | endswith("probe.exe")))'
