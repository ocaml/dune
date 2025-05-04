Dune builds should not visit folders ignored via (dirs)

  $ mkdir test
  $ cd test

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ mkdir -p src/foo src/should_not_check_this
  $ cat >src/dune <<EOF
  > (dirs foo)
  > (include_subdirs unqualified)
  > (library (name foo))
  > EOF
  $ touch src/foo/a.ml
  $ touch src/should_not_check_this/b.ml

  $ strace -o ../log dune build

  $ cat ../log | grep -e "should_not_check_this"
  stat("src/should_not_check_this", {st_mode=S_IFDIR|0755, st_size=4096, ...}) = 0
