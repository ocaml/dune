Demonstrate sandbox events:

  $ make_dune_project "3.22"

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps (sandbox always))
  >  (action (bash "true")))
  > EOF

  $ dune build @foo

  $ dune trace cat | jq_dune '
  >   select(.cat == "sandbox")
  > | del(.ts,.dur, .args.queued)
  > | censorDigestDir
  > '
  {
    "cat": "sandbox",
    "name": "create",
    "args": {
      "loc": "dune:1",
      "dir": "_build/.sandbox/$DIGEST"
    }
  }
  {
    "cat": "sandbox",
    "name": "extract",
    "args": {
      "loc": "dune:1",
      "dir": "_build/.sandbox/$DIGEST"
    }
  }
  {
    "cat": "sandbox",
    "name": "destroy",
    "args": {
      "loc": "dune:1",
      "dir": "_build/.sandbox/$DIGEST"
    }
  }

A second execution of the same rule currently reuses its sandbox path.

  $ cat >>dune <<'EOF'
  > (rule
  >  (target bar)
  >  (deps (sandbox always) (universe))
  >  (action (with-stdout-to bar (echo bar))))
  > EOF
  $ jq_filter='select(.cat == "sandbox" and .name == "create") | .args.dir'
  $ dune build bar
  $ first=$(dune trace cat | jq_dune -r "$jq_filter")
  $ dune build bar
  $ second=$(dune trace cat | jq_dune -r "$jq_filter")
  $ test -n "$first" && test -n "$second"
  $ test "${first##*/}" = "${second##*/}"
