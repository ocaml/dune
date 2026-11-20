Changing an export from a locked formatter's dependency should invalidate
formatting even when the source, configuration and executable are unchanged.

  $ make_dune_project 3.25
  $ make_lockdir
  $ source_lock_dir="$dev_tool_lock_dir"
  $ make_lockdir
  $ make_lockpkg ocamlformat <<'EOF'
  > (version 0.26.2)
  > (depends style)
  > (install
  >  (progn
  >   (system "\| cat > %{bin}/ocamlformat <<'SCRIPT'
  >           "\| #!/bin/sh
  >           "\| echo "(* $FMT_STYLE *)"
  >           "\| SCRIPT
  >   )
  >   (run chmod +x %{bin}/ocamlformat)))
  > EOF
  $ set_style() {
  >   make_lockpkg style <<EOF
  > (version 0.0.1)
  > (exported_env (= FMT_STYLE $1))
  > EOF
  > }
  $ echo 'version=0.26.2' >.ocamlformat
  $ echo '(* red *)' >cached.ml
  $ set_style red
  $ dune build @fmt
  $ exe=$(dune tools which ocamlformat)
  $ cp "$exe" old-formatter

The cached file is still accepted after the style changes to blue.

  $ set_style blue
  $ dune build @fmt
  $ cmp "$exe" old-formatter

A new file with identical contents gets the blue correction in the same build.

  $ cp cached.ml new.ml
  $ dune build @fmt
  File "new.ml", line 1, characters 0-0:
  --- new.ml
  +++ new.ml.corrected
  @@ -1 +1 @@
  -(* red *)
  +(* blue *)
  [1]
