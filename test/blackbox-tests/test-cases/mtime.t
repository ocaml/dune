  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (write-file x.gen new-value))
  > 
  > (rule
  >  (alias default)
  >  (action
  >   (diff x x.gen)))
  > EOF

  $ (for i in $(seq 1 100); do
  >   dune clean
  >   printf old-value > x
  >   dune build --auto-promote >/dev/null 2>&1
  >   dune build
  > done) 2>&1 |grep -c Error
