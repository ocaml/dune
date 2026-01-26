Test if setup scripts are visible in test directory

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ cat > secret.sh << 'EOF'
  > MY_SECRET="should_not_be_visible"
  > EOF

  $ cat > dune << EOF
  > (cram
  >  (shell bash)
  >  (setup_scripts secret.sh))
  > EOF

  $ cat > check.t << 'EOF'
  >   $ shopt -s nullglob
  >   > set -- *.sh
  >   > shopt -u nullglob
  >   $ if (( $# )); then
  >   >   echo "have .sh files"
  >   > else
  >   >   echo "no .sh files"
  >   > fi
  >   no .sh files
  > EOF

  $ dune runtest
