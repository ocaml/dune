Things should be the same whether dependencies are specified or not.

  $ . ./helpers.sh

  $ make_lockdir

If we have a package we depend on

  $ mkdir dependency-source
  $ cat > ${default_lock_dir}/dependency.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/dependency-source))
  > EOF

And we have a package we want to build

  $ mkdir test-source
  $ cat > ${default_lock_dir}/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (system "command -v cat > /dev/null 2>&1 || echo no cat"))
  > EOF
  $ build_pkg test

Now it fails since adding the dependency modified PATH.

  $ cat > ${default_lock_dir}/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > ; adding deps breaks cat
  > (depends dependency)
  > (build
  >  (system "command -v cat > /dev/null 2>&1 || echo no cat"))
  > EOF
  $ build_pkg test
