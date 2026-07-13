A source change used to be missed when the persistent digest database believed
the source file's cached digest was still valid. The rule cache then reused a
target built from the old source contents until _build was removed.

  $ make_dune_project 3.25
  $ cat > dune <<'EOF'
  > (rule
  >  (deps source)
  >  (target copy)
  >  (action (copy %{deps} %{target})))
  > EOF
  $ printf 'old-source-contents\n' > source

Populate the persistent digest database and the rule cache.

  $ dune build copy
  $ cat _build/default/copy
  old-source-contents

Now change the source contents while preserving the metadata that used to be
checked by Dune's digest database. The ctime still changes, so Dune should no
longer trust the cached digest.

  $ cp -p source source.stamp
  $ printf 'new-source-contents\n' > source
  $ touch -r source.stamp source

The source changed, so Dune must recompute the digest and rebuild the target
without requiring _build to be removed.

  $ dune build copy
  $ cat _build/default/copy
  new-source-contents
