Attempt to create a directory with chdir + with-stdout-to:

  $ make_directory_targets_project 3.1

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir output))
  >  (deps (sandbox always))
  >  (action
  >   (progn
  >    (chdir output
  >     (with-stdout-to x (echo foobar))))))
  > EOF

  $ dune build foobar/
  File "dune", line 7, characters 20-21:
  7 |     (with-stdout-to x (echo foobar))))))
                          ^
  Error: This action has targets in a different directory than the current one,
  this is not allowed by dune at the moment:
  - output/x
  [1]

A sequence may create its directory before changing into it. The directory
should not be created until the action reaches `chdir`.

  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir ordered))
  >  (deps (sandbox always))
  >  (action
  >   (progn
  >    (system "\| if test -d ordered; then echo precreated; exit 1; fi
  >            "\| mkdir ordered
  >    )
  >    (chdir ordered (run touch result)))))
  > EOF
  $ dune build ordered
  $ test -f _build/default/ordered/result
