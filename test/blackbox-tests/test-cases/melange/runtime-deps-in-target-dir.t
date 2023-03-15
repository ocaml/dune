
  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF
  $ mkdir output

  $ cat > output/foo.txt <<EOF
  > hello foo
  > EOF
  $ cat > dune << EOF
  > (melange.emit
  >  (target output)
  >  (alias melange)
  >  (runtime_deps output/foo.txt))
  > EOF

  $ dune build @melange 2>&1 | grep -A6 "internal dependency cycle"
    ("internal dependency cycle",
    { frames =
        [ ("load-dir", In_build_dir "default/output")
        ; ("build-file", In_build_dir "default/output/foo.txt")
        ; ("<unnamed>", ())
        ]
    })


  $ ls _build/default/output
  ls: cannot access '_build/default/output': No such file or directory
  [2]
  $ ls _build/default/output/output
  ls: cannot access '_build/default/output/output': No such file or directory
  [2]
