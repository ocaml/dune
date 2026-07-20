On Windows, artifact lookup uses extensionless executable names while the
layout must use the actual installed filenames. Non-executable bin entries
retain their declared extensions.

  $ make_dune_project_with_package 3.24 mypkg
  $ mkdir producer

  $ cat >producer/launcher.ml <<'EOF'
  > let () = print_endline "hello from launcher"
  > EOF

  $ cat >producer/tool.cmd <<'EOF'
  > @echo off
  > echo hello from tool
  > EOF

  $ cat >producer/dune <<'EOF'
  > (executable
  >  (name launcher)
  >  (public_name launcher)
  >  (package mypkg))
  > (install
  >  (package mypkg)
  >  (section bin)
  >  (files tool.cmd))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (deps %{bin:launcher} %{bin:tool.cmd})
  >  (action
  >   (progn
  >    (with-stdout-to launcher.out (system "launcher"))
  >    (with-stdout-to tool.out (system "tool.cmd")))))
  > EOF

On native Windows, both entries are callable by their lookup names:

  $ dune build launcher.out tool.out
  $ cat _build/default/launcher.out
  hello from launcher
  $ cat _build/default/tool.out
  hello from tool

The layout uses the installed filenames, including [.exe] only for the
executable:

  $ ls _build/install/default/.binaries/*
  launcher.exe
  tool.cmd
