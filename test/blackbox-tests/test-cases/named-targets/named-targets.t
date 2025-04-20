# Basic test for multiple targets with named targets
  $ echo '(lang dune 3.8)' > dune-project
  $ cat > dune << 'EOF'
  > (rule
  >  (targets (output.txt as primary) secondary.log)
  >  (deps dune-project)
  >  (action
  >   (progn
  >    (with-stdout-to %{targets:primary} (echo "Primary content"))
  >    (with-stdout-to %{targets:secondary.log} (echo "Log content"))
  >   )
  >  )
  > )
  > EOF
  $ dune build
  $ test -f _build/default/output.txt
  $ test -f _build/default/secondary.log
  $ cat _build/default/output.txt
  Primary content
  $ cat _build/default/secondary.log
  Log content