Since Dune 3.18, we support `env_vars` as an alias for `env-vars` for naming
consistency.

  $ cat > dune-project << EOF
  > (lang dune 3.18)
  > EOF

We check the rule is evaluated:

  $ cat > dune << 'EOF'
  > (env
  >  (_   (env_vars (VAR default ))))
  > (rule
  >  (alias runtest)
  >  (action (system "echo $VAR")))
  > EOF

  $ dune runtest
  default
