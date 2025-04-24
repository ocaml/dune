# Test for named targets in Dune rules
  $ echo '(lang dune 3.8)' > dune-project
  $ cat > dune << 'EOF'
  > (rule
  >  (targets output.txt secondary.log)
  >  (action
  >   (progn
  >    (with-stdout-to output.txt (echo "This is the first target: %{target}"))
  >    (with-stdout-to secondary.log (echo "First target: %{target}, Named target: %{target:output.txt}"))
  >   )
  >  )
  > )
  > EOF
  $ dune build
  $ test -f _build/default/output.txt
  $ test -f _build/default/secondary.log
  $ cat _build/default/output.txt
  This is the first target: output.txt
  $ cat _build/default/secondary.log
  First target: output.txt, Named target: output.txt

# Test for named targets with explicit naming
  $ cat > dune << 'EOF'
  > (rule
  >  (targets (:main output.txt) (:log secondary.log) third.data)
  >  (action
  >   (progn
  >    (with-stdout-to %{main} (echo "Main file, first target: %{target}, Log file: %{target:log}"))
  >    (with-stdout-to %{log} (echo "Log file, first target: %{target}, Main file: %{target:main}"))
  >    (with-stdout-to third.data (echo "Third file, Main: %{target:main}, Log: %{target:log}, First: %{target}"))
  >   )
  >  )
  > )
  > EOF
  $ dune build
  $ test -f _build/default/output.txt
  $ test -f _build/default/secondary.log
  $ test -f _build/default/third.data
  $ cat _build/default/output.txt
  Main file, first target: output.txt, Log file: secondary.log
  $ cat _build/default/secondary.log
  Log file, first target: output.txt, Main file: output.txt
  $ cat _build/default/third.data
  Third file, Main: output.txt, Log: secondary.log, First: output.txt