# Basic test for multiple targets
  $ echo '(lang dune 3.8)' > dune-project
  $ cat > dune << 'EOF'
  (rule
   (targets output.txt secondary.log)
   (action
    (progn
     (with-stdout-to output.txt (echo "Primary content"))
     (with-stdout-to secondary.log (echo "Log content"))
     (mode fallback)  ; Disable sandboxing
    )
   )
  )
  EOF
  $ dune build
  $ test -f _build/default/output.txt
  $ test -f _build/default/secondary.log
  $ cat _build/default/output.txt
  Primary content
  $ cat _build/default/secondary.log
  Log content