Test that we can set variables

  $ . ./helpers.sh

  $ make_lockdir
  $ cat >dune.lock/test.pkg <<EOF
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

  $ cat >dune.lock/usetest.pkg <<EOF
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
  share path: ../../test/target/share/test
  version: 1.2.3

  $ show_pkg_cookie test
  { files = map {}
  ; variables =
      [ ("abool", Bool true)
      ; ("astring", String "foobar")
      ; ("somestrings", Strings [ "foo"; "bar" ])
      ; ("version", String "1.2.3")
      ]
  }

Now we demonstrate we get a proper error from invalid .config files:

  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\| cat >test.config <<EOF
  >          "\| this is dummy text
  >          "\| EOF
  >  ))
  > EOF

  $ build_pkg test 2>&1 | sed 's/File .*:/File $REDACTED:/'
  Error:
  File $REDACTED:
  1 | this is dummy text
           ^^
  Error parsing test.config
  Reason: Parse error
  -> required by _build/_private/default/.pkg/test/target
