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
  >  include "dune";
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
