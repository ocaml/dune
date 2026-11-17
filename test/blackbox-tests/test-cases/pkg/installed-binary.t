Test that installed binaries are visible in dependent packages

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\| echo "#!/bin/sh\necho from test package" > foo;
  >          "\| chmod +x foo;
  >          "\| touch libxxx lib_rootxxx;
  >          "\| cat >test.install <<EOF
  >          "\| bin: [ "foo" ]
  >          "\| lib: [ "libxxx" ]
  >          "\| lib_root: [ "lib_rootxxx" ]
  >          "\| share_root: [ "lib_rootxxx" ]
  >          "\| EOF
  >  ))
  > EOF

  $ make_lockpkg usetest <<EOF
  > (version 0.0.1)
  > (depends test)
  > (build
  >  (progn
  >   (run foo)
  >   (run mkdir -p %{prefix})))
  > EOF

  $ build_pkg usetest
  from test package

  $ show_pkg_targets test
  
  /bin
  /bin/foo
  /cookie
  /lib
  /lib/lib_rootxxx
  /lib/test
  /lib/test/libxxx
  /share
  /share/lib_rootxxx
  $ show_pkg_cookie test
  { files =
      [ (LIB,
         [ In_build_dir "_private/default/.pkg/test/target/lib/test/libxxx" ])
      ; (LIB_ROOT,
         [ In_build_dir "_private/default/.pkg/test/target/lib/lib_rootxxx" ])
      ; (BIN, [ In_build_dir "_private/default/.pkg/test/target/bin/foo" ])
      ; (SHARE_ROOT,
         [ In_build_dir "_private/default/.pkg/test/target/share/lib_rootxxx" ])
      ]
  ; variables = []
  }

It should also be visible in the workspace:

  $ make_dune_project 3.9

  $ cat >dune <<EOF
  > (rule
  >  (with-stdout-to testout (run %{bin:foo})))
  > EOF

  $ dune build ./testout && cat _build/default/testout
  from test package

Two packages install the same command. A recipe using the shell should select
the same provider if an intermediate lock entry merely lists its dependencies
in the opposite order. Which independent provider wins is not part of the test.

  $ export DUNE_CACHE=disabled
  $ make_lockpkg other <<'EOF'
  > (version 1)
  > EOF
  $ make_lockpkg_file other foo <<'EOF'
  > #!/bin/sh
  > echo from other package
  > EOF
  $ echo 'bin: [ "foo" ]' > dune.lock/other.files/other.install
  $ middle() {
  >   make_lockpkg middle <<EOF
  > (version 1)
  > (depends $1 $2)
  > EOF
  > }
  $ make_lockpkg usetest <<'EOF'
  > (version 1)
  > (depends middle)
  > (install (system "echo selecting; foo > %{prefix}/selected"))
  > EOF
  $ middle test other
  $ target=$(get_build_pkg_dir usetest)/target
  $ dune build "$target"
  selecting
  $ first=$(cat "$target/selected")
  $ echo "$first" | grep -Eq '^from (other|test) package$'

Both installations are inputs. An unused directory on the inherited PATH is not.

  $ a=$(get_build_pkg_dir test)/target
  $ b=$(get_build_pkg_dir other)/target
  $ dune rules --format=json "$target" | jq_dune -c --arg a "$a" --arg b "$b" '
  >   .[] | {providers: (ruleHasDepFile($a) and ruleHasDepFile($b)),
  >          path: (.deps | index({Env: "PATH"}) != null)}'
  {"providers":true,"path":false}
  $ mkdir ambient-bin
  $ PATH="$PWD/ambient-bin:$PATH" dune build "$target"

Swap the two providers without changing their files or the consuming recipe.
Known regression: a fresh build changes provider, but the incremental build
keeps the original one.

  $ middle other test
  $ target=$(get_build_pkg_dir usetest)/target
  $ dune build "$target" > log 2>&1
  $ fresh=_fresh/${target#_build/}
  $ dune build --build-dir _fresh "$fresh" > log 2>&1
  $ selected=$(cat "$target/selected")
  $ test "$selected" = "$(cat "$fresh/selected")"
  [1]
  $ test "$selected" = "$first"

Adding a dependency edge between the same two providers really changes the
graph. Even if neither helper changes, incremental and fresh builds must agree.

  $ echo '(depends test)' >> dune.lock/other.pkg
  $ target=$(get_build_pkg_dir usetest)/target
  $ dune build "$target" > log 2>&1
  $ fresh=_fresh_edge/${target#_build/}
  $ dune build --build-dir _fresh_edge "$fresh" > log 2>&1
  $ test "$(cat "$target/selected")" = "$(cat "$fresh/selected")"
