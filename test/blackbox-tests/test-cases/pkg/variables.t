Test that we can set variables

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\| cat >test.config <<EOF
  >          "\| opam-version: "2.0"
  >          "\| variables {
  >          "\|   abool: true
  >          "\|   astring: "foobar"
  >          "\|   somestrings: ["foo" "bar"]
  >          "\|   version: "1.2.3"
  >          "\| }
  >          "\| EOF
  >  ))
  > EOF

  $ make_lockpkg usetest <<EOF
  > (version 0.0.1)
  > (depends test)
  > (build
  >  (progn
  >   (system "\| echo abool: %{pkg:test:abool}
  >           "\| echo astring: %{pkg:test:astring}
  >           "\| echo somestrings: %{pkg:test:somestrings}
  >           "\| echo share path: %{pkg:test:share}
  >           "\| echo version: %{pkg:test:version}
  >   )
  >   (run mkdir -p %{prefix})))
  > EOF

  $ build_pkg usetest
  abool: true
  astring: foobar
  somestrings: foo bar
  share path: ../../test.0.0.1-4720c73ae6ba1444af0517c28d2b366e/target/share/test
  version: 1.2.3

  $ show_pkg_cookie test
  { files = []
  ; variables =
      [ ("abool", Bool true)
      ; ("astring", String "foobar")
      ; ("somestrings", Strings [ "foo"; "bar" ])
      ; ("version", String "1.2.3")
      ]
  }

Now we demonstrate we get a proper error from invalid .config files:

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\| cat >test.config <<EOF
  >          "\| this is dummy text
  >          "\| EOF
  >  ))
  > EOF

  $ build_pkg test 2>&1 | dune_cmd subst 'File .*:' 'File $REDACTED:'
  Error:
  File $REDACTED:
  1 | this is dummy text
           ^^
  Error parsing test.config
  Reason: Parse error
  -> required by
     _build/_private/default/.pkg/test.0.0.1-69ac79828345d1ca458538ce94b5c970/target
