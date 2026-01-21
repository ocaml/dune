Test that opam variables in setenv values are properly expanded.

This test makes sure that opam variables like %{share}% in setenv values
are expanded.

  $ mkrepo

Make a package with a setenv that uses opam variables like %{share}%

  $ mkpkg with-setenv-vars <<EOF
  > build: [ "sh" "-c" "mkdir -p %{share}%/mydata && echo hello > %{share}%/mydata/file.txt" ]
  > install: [ "sh" "-c" "true" ]
  > setenv: [
  >  [MY_DATA_DIR = "%{share}%/mydata"]
  > ]
  > EOF

Make another package that depends on that and checks if the env var was expanded correctly

  $ mkpkg uses-setenv-vars <<'EOF'
  > depends: [ "with-setenv-vars" ]
  > build: [
  >  [ "sh" "-c" "echo MY_DATA_DIR=$MY_DATA_DIR" ]
  >  [ "sh" "-c" "cat $MY_DATA_DIR/file.txt" ]
  > ]
  > EOF

  $ solve uses-setenv-vars
  Solution for dune.lock:
  - uses-setenv-vars.0.0.1
  - with-setenv-vars.0.0.1

  $ cat ${default_lock_dir}/with-setenv-vars.0.0.1.pkg | grep "exported_env" -A 1
  (exported_env
   (= MY_DATA_DIR "\%{share}%/mydata"))

Build the first package
  $ build_pkg with-setenv-vars

The second package should be able to access the data file via the exported env var.

However instead, it gets the literal unexpanded string "%{share}%/mydata"

  $ build_pkg uses-setenv-vars
  MY_DATA_DIR=%{share}%/mydata
  File "dune.lock/uses-setenv-vars.0.0.1.pkg", line 8, characters 10-12:
  8 |      (run sh -c "cat $MY_DATA_DIR/file.txt"))))))
                ^^
  Error: Logs for package uses-setenv-vars
  cat: %{share}%/mydata/file.txt: No such file or directory
  
  [1]

