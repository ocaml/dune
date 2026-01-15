Test the (vendor ...) stanza for selective library exposure from vendored directories.

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package (name myapp))
  > EOF

Create a duniverse-style vendored directory structure:

  $ mkdir -p duniverse/fmt.0.9.0
  $ cat >duniverse/fmt.0.9.0/dune-project <<EOF
  > (lang dune 3.0)
  > (package (name fmt))
  > EOF

  $ cat >duniverse/fmt.0.9.0/dune <<EOF
  > (library
  >  (name fmt)
  >  (modules fmt)
  >  (public_name fmt))
  > (library
  >  (name fmt_tty)
  >  (modules fmt_tty)
  >  (public_name fmt.tty))
  > (library
  >  (name fmt_cli)
  >  (modules fmt_cli)
  >  (public_name fmt.cli))
  > EOF

  $ cat >duniverse/fmt.0.9.0/fmt.ml <<EOF
  > let greeting = "Hello from fmt"
  > EOF

  $ cat >duniverse/fmt.0.9.0/fmt_tty.ml <<EOF
  > let tty_msg = "TTY output"
  > EOF

  $ cat >duniverse/fmt.0.9.0/fmt_cli.ml <<EOF
  > let cli_msg = "CLI output"
  > EOF

Use vendored_dirs with vendor stanza to expose only specific libraries:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0 (libraries fmt fmt.tty))
  > EOF

Create main app that uses the vendored library:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries fmt))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline Fmt.greeting
  > EOF

Build should succeed with allowed library:

  $ dune build main.exe

Test that vendor stanza is parsed with package field:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0
  >  (libraries fmt fmt.tty)
  >  (package fmt))
  > EOF

  $ dune build main.exe

Test multiple vendor stanzas for same directory (one per package):

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0 (package fmt) (libraries fmt))
  > (vendor fmt.0.9.0 (package fmt) (libraries fmt.tty))
  > EOF

  $ dune build main.exe

Test vendor stanza with all fields:

  $ cat >duniverse/dune <<EOF
  > (vendored_dirs *)
  > (vendor fmt.0.9.0
  >  (libraries fmt fmt.tty)
  >  (package fmt))
  > EOF

  $ dune build main.exe
