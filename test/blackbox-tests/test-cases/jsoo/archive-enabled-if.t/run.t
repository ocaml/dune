Generating jsoo archive rules for a library that shares its name with
another library in the same directory, the two being distinguished by
mutually exclusive enabled_if clauses. The rules must be generated for
the enabled stanza, not the first one by position.

  $ dune build bin/main.bc.js --profile dev 2>&1 | head -12
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("modules_and_obj_dir: failed lookup",
     { keys =
         [ { name = "foo"; loc = "foo/dune:6"; src_dir = In_source_tree "foo" }
         ]
     ; for_ =
         Library
           { name = "foo"; loc = "foo/dune:2"; src_dir = In_source_tree "foo" }
     })
  [1]
  $ node ./_build/default/bin/main.bc.js 2> /dev/null
  [1]
