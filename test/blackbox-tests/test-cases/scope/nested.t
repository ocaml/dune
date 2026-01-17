Test nested scope stanzas (scope stanza in a subdirectory)

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

Create a nested directory structure:
- duniverse/

  $ mkdir -p duniverse/scopetest_fmt.0.9.0 duniverse/scopetest_cohttp.6.0.0

Create scopetest_fmt library:
  $ cat > duniverse/scopetest_fmt.0.9.0/dune-project << EOF
  > (lang dune 3.22)
  > (package (name scopetest_fmt))
  > EOF

  $ cat > duniverse/scopetest_fmt.0.9.0/dune << EOF
  > (library
  >  (name scopetest_fmt)
  >  (modules scopetest_fmt)
  >  (public_name scopetest_fmt))
  > (library
  >  (name scopetest_fmt_tty)
  >  (modules scopetest_fmt_tty)
  >  (public_name scopetest_fmt.tty))
  > EOF

  $ cat > duniverse/scopetest_fmt.0.9.0/scopetest_fmt.ml << EOF
  > let msg = "scopetest_fmt"
  > EOF

  $ cat > duniverse/scopetest_fmt.0.9.0/scopetest_fmt_tty.ml << EOF
  > let msg = "scopetest_fmt.tty"
  > EOF

Create scopetest_cohttp library:
  $ cat > duniverse/scopetest_cohttp.6.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (package (name scopetest_cohttp))
  > EOF

  $ cat > duniverse/scopetest_cohttp.6.0.0/dune << EOF
  > (library
  >  (name scopetest_cohttp)
  >  (modules scopetest_cohttp)
  >  (public_name scopetest_cohttp))
  > (library
  >  (name scopetest_cohttp_async)
  >  (modules scopetest_cohttp_async)
  >  (public_name scopetest_cohttp.async))
  > EOF

  $ cat > duniverse/scopetest_cohttp.6.0.0/scopetest_cohttp.ml << EOF
  > let msg = "scopetest_cohttp"
  > EOF

  $ cat > duniverse/scopetest_cohttp.6.0.0/scopetest_cohttp_async.ml << EOF
  > let msg = "scopetest_cohttp.async"
  > EOF

Create scope stanzas in duniverse/dune to expose packages:
  $ cat > duniverse/dune << EOF
  > (scope
  >  (dir scopetest_fmt.0.9.0)
  >  (packages scopetest_fmt))
  > (scope
  >  (dir scopetest_cohttp.6.0.0)
  >  (packages scopetest_cohttp))
  > EOF

Create main app that uses the exposed libraries:
  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries scopetest_fmt scopetest_fmt.tty scopetest_cohttp scopetest_cohttp.async))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline (Scopetest_fmt.msg ^ " " ^ Scopetest_cohttp.msg)
  > EOF

Build should succeed - all libraries from the exposed packages are visible:
  $ dune exec ./main.exe
  scopetest_fmt scopetest_cohttp

Test deeply nested scope: scope stanza in deeper directory
  $ mkdir -p duniverse/extras
  $ cat > duniverse/extras/dune-project << EOF
  > (lang dune 3.22)
  > EOF

Add another library in extras:
  $ mkdir -p duniverse/extras/scopetest_lwt.5.0.0
  $ cat > duniverse/extras/scopetest_lwt.5.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (package (name scopetest_lwt))
  > EOF

  $ cat > duniverse/extras/scopetest_lwt.5.0.0/dune << EOF
  > (library
  >  (name scopetest_lwt)
  >  (public_name scopetest_lwt))
  > EOF

  $ cat > duniverse/extras/scopetest_lwt.5.0.0/scopetest_lwt.ml << EOF
  > let msg = "scopetest_lwt"
  > EOF

Create scope stanza in duniverse/extras/dune:
  $ cat > duniverse/extras/dune << EOF
  > (scope
  >  (dir scopetest_lwt.5.0.0)
  >  (packages scopetest_lwt))
  > EOF

Main app should be able to use scopetest_lwt through the nested scope:
  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries scopetest_lwt))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline Scopetest_lwt.msg
  > EOF

  $ dune exec ./main.exe
  scopetest_lwt
