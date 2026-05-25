A workspace package and a lock-dir package both install a binary
named "foo". A consumer lock-dir package depends on both; the
workspace install layout is prepended to consumer's PATH, so when
consumer's build action runs "foo" it picks up the workspace binary,
not the lock-dir one. This pins the layout-vs-lockdir precedence
on path-like env vars.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name ws-tool))
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (executable
  >  (name main)
  >  (package ws-tool)
  >  (public_name foo))
  > EOF
  $ cat > src/main.ml <<EOF
  > let () = print_endline "from-workspace"
  > EOF

The lock dir contains lock-tool, which installs its own "foo" binary,
and consumer, which depends on both:

  $ make_lockdir
  $ make_lockpkg lock-tool <<EOF
  > (version 0.0.1)
  > (install
  >  (system "mkdir -p %{bin} && printf '#!/bin/sh\necho from-lockdir\n' > %{bin}/foo && chmod +x %{bin}/foo"))
  > EOF
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends lock-tool ws-tool)
  > (build (system "foo > which.txt"))
  > (install
  >  (system "mkdir -p %{share}/consumer && cp which.txt %{share}/consumer/"))
  > EOF

  $ write_lockdir_consumer_rule

  $ dune build out

The workspace binary wins:

  $ find _build -name 'which.txt' -exec cat {} \;
  from-workspace
