A source change can be missed when the persistent digest database believes the
source file's cached digest is still valid. The rule cache then reuses a target
built from the old source contents. Removing _build drops the stale digest and
rule caches, so the same build sees the changed source.

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

Now change the source contents, but preserve the file metadata that Dune's
digest database uses to decide whether the cached digest is still valid.

  $ cp -p source source.stamp
  $ printf 'new-source-contents\n' > source
  $ touch -r source.stamp source

The source changed, but Dune incorrectly reuses the stale cached digest and
keeps the target built from the old contents.

  $ dune build copy
  $ cat _build/default/copy
  old-source-contents

Removing _build removes the stale digest and rule caches, so the same build now
sees the changed source file.

  $ rm -rf _build
  $ dune build copy
  $ cat _build/default/copy
  new-source-contents
