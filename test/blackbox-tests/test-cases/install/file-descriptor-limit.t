Installing many independent files must not exhaust the file descriptor limit.
The limit applies only to the installer, not to building the package.

  $ make_dune_project_with_package 3.25 foo
  $ mkdir files
  $ for i in $(seq 1 300); do
  >   printf 'payload\n' >"files/file-$i"
  > done
  $ cat >dune <<'EOF'
  > (install
  >  (section share)
  >  (files (glob_files files/*)))
  > EOF
  $ dune build @install
  $ (ulimit -n 128; dune install --prefix prefix >install.log 2>&1)
  $ test "$(find prefix/share/foo -type f | wc -l)" -eq 300
  $ grep -q 'Too many open files' install.log
  [1]
