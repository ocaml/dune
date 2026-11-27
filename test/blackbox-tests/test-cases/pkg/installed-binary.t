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
      [ (LIB, [ In_build_dir "default/.pkg/test/_install/lib/test/libxxx" ])
      ; (LIB_ROOT,
         [ In_build_dir "default/.pkg/test/_install/lib/lib_rootxxx" ])
      ; (BIN, [ In_build_dir "default/.pkg/test/_install/bin/foo" ])
      ; (SHARE_ROOT,
         [ In_build_dir "default/.pkg/test/_install/share/lib_rootxxx" ])
      ]
  ; variables = []
  }

It should also be visible in a workspace package that declares the dependency:

  $ make_dune_project 3.24
  $ echo '(package (name consumer) (allow_empty) (dir .) (depends test))' >> dune-project

  $ cat >dune <<EOF
  > (rule
  >  (with-stdout-to testout (run %{bin:foo})))
  > EOF

  $ dune build ./testout && cat _build/default/testout
  from test package

Two packages install the same command. A package that depends on both cannot
be built: choosing a provider would make the order of its dependencies matter.

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
  $ target=$(get_build_pkg_dir usetest)/_install
  $ dune build "$target"
  Error: "other" and "test" both install "foo" to section bin.
  The lib_root, share_root, and libexec_root sections install directly to the
  section root with no per-package subdirectory, so file names must be unique
  across the set of packages a rule depends on.
  -> required by _build/default/.pkg/middle/_install/cookie
  -> required by _build/default/.pkg/usetest/_install
  Hint: Rename one of the install entries.
  [1]

The order of the dependencies does not change the error.

  $ middle other test
  $ dune build "$target" 2>&1 | head -1
  Error: "other" and "test" both install "foo" to section bin.
  [1]
