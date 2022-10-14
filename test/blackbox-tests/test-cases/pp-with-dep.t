  $ cat > dune-project <<EOF
  > (lang dune 3.4)
  > (name dune_bug)
  > EOF

  $ touch bug.ml

  $ cat > pp.sh <<EOF
  > #!/bin/bash
  > echo "let _ = Printf.printf \"Hello, world!\\n%!\""
  > EOF

  $ chmod +x pp.sh

  $ cat > dune <<EOF
  > (executable
  >  (preprocess (per_module ((action (run ./pp.sh)) bug)))
  >  (preprocessor_deps (file pp.sh))
  >  (name bug))
  > EOF

Works as expected if the files on which the preprocessor depends are at the
project root.

  $ dune build

However, things go wrong if they are not.

  $ dune clean

  $ mkdir src/
  $ mv bug.ml dune pp.sh src/

  $ dune build
