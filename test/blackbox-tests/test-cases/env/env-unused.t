  $ cat > dune-project << EOF
  > (lang dune 3.3)
  > EOF

(env) is evaluated sequentially:

  $ cat > dune << 'EOF'
  > (rule
  >  (alias runtest)
  >  (action (system "echo $VAR")))
  > 
  > (env
  >  (_   (env-vars (VAR default )))
  >  (dev (env-vars (VAR specific))))
  > EOF

A warning is displayed when there are unreachable cases.

  $ dune runtest
  File "dune", line 5, characters 0-71:
  5 | (env
  6 |  (_   (env-vars (VAR default )))
  7 |  (dev (env-vars (VAR specific))))
  Warning: This env stanza contains rules after a wildcard rule. These are
  going to be ignored.
  default

In 3.4, this warning becomes fatal.

  $ cat > dune-project << EOF
  > (lang dune 3.4)
  > EOF

  $ dune runtest
  File "dune", line 5, characters 0-71:
  5 | (env
  6 |  (_   (env-vars (VAR default )))
  7 |  (dev (env-vars (VAR specific))))
  Error: This env stanza contains rules after a wildcard rule. These are going
  to be ignored.
  [1]
