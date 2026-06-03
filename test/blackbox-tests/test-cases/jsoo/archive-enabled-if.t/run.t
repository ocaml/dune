Generating jsoo archive rules for a library that shares its name with
another library in the same directory, the two being distinguished by
mutually exclusive enabled_if clauses. The rules must be generated for
the enabled stanza, not the first one by position.

  $ dune build bin/main.bc.js --profile dev 2>&1 | head -12
  $ node ./_build/default/bin/main.bc.js 2> /dev/null
  hello
