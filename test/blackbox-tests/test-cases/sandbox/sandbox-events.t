Demonstrate sandbox events:

  $ make_dune_project "3.22"

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps (sandbox always))
  >  (action (system "true")))
  > EOF

  $ dune build @foo

  $ dune trace cat | jq '
  >   select(.cat == "sandbox")
  > | del(.ts,.dur, .args.queued)
  > | .args.dir |= (if . then sub("[0-9a-f]{32}"; "$DIGEST") else . end)
  > '
  {
    "cat": "sandbox",
    "name": "create-sandbox",
    "args": {
      "loc": "dune:1",
      "dir": null
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
