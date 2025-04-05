  $ cat > dune-project <<'EOF'
  > (lang dune 3.8)
  > EOF

  # Test 1: Basic named targets
  $ cat > dune <<'EOF'
> (rule
>  (targets output.txt secondary.log)
>  (action
>   (progn
>    (with-stdout-to %{targets:main} (echo "Primary content"))
>    (with-stdout-to %{targets:log} (echo "Log content"))
>   )
>  )
> )
> EOF


  $ dune build
  $ ls _build/default
  output.txt
  secondary.log

  # Test 2: Verify contents
  $ printf "Primary content" > output.expected
  $ printf "Log content" > secondary.expected
  $ diff _build/default/output.txt output.expected
  $ diff _build/default/secondary.log secondary.expected

  # Test 3: Simple targets
  $ cat > dune <<'EOF'
> (rule
>  (targets output.txt secondary.log)
>  (action
>   (progn
>    (with-stdout-to output.txt (echo "Primary"))
>    (with-stdout-to secondary.log (echo "Log"))
>   )
>  )
> )
> EOF


  $ dune build
  [0]