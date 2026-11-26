The enabled_if stanza is available from Dune language 3.26.

  $ echo '(lang dune 3.25)' > dune-project
  $ echo '(enabled_if false (rule (write-file result skipped)))' > dune
  $ dune build
  File "dune", line 1, characters 0-53:
  1 | (enabled_if false (rule (write-file result skipped)))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'enabled_if' is only available since version 3.26 of the dune
  language. Please update your dune-project file to have (lang dune 3.26).
  [1]

Enabled groups contribute all their stanzas in the current context, and may
contain nested groups. A disabled body is not decoded or expanded.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.26)
  > (context (default (name debug) (profile dev)))
  > (context (default (name optimized) (profile release)))
  > EOF
  $ cat > dune <<'EOF'
  > (enabled_if (= %{profile} dev)
  >  (rule (write-file result debug))
  >  (rule (copy result extra)))
  > (enabled_if (= %{profile} release)
  >  (rule (write-file result release))
  >  (enabled_if (= %{context_name} optimized)
  >   (rule (copy result extra))))
  > (enabled_if false
  >  (plugin.not_defined)
  >  (dynamic_include missing.inc)
  >  (not_a_stanza))
  > EOF
  $ dune build _build/debug/extra _build/optimized/extra
  $ cat _build/debug/extra; echo
  debug
  $ cat _build/optimized/extra; echo
  release

A guarded static stanza, such as a library, is visible to the rest of the
build, not just to rule generation in its directory.

  $ cat > dune <<'EOF'
  > (enabled_if (= %{profile} release)
  >  (library (name selected) (modules selected)))
  > (executable (name main) (modules main) (libraries selected))
  > EOF
  $ echo 'let message = "selected"' > selected.ml
  $ echo 'let () = print_endline Selected.message' > main.ml
  $ dune build _build/optimized/main.exe
  $ ./_build/optimized/main.exe
  selected

The condition also applies when loading generated stanzas. A dynamic include
can both be guarded and produce guarded rules.

  $ mkdir generated inputs
  $ cat > generated/dune <<'EOF'
  > (enabled_if (= %{profile} release)
  >  (dynamic_include ../inputs/rules.inc))
  > EOF
  $ cat > inputs/rules.inc <<'EOF'
  > (enabled_if (= %{context_name} optimized)
  >  (rule (write-file result generated)))
  > (enabled_if false (dynamic_include missing.inc))
  > EOF
  $ dune build _build/optimized/generated/result
  $ cat _build/optimized/generated/result; echo
  generated

Source-tree stanzas cannot be included conditionally.

  $ echo '(enabled_if true (dirs :standard))' > dune
  $ dune build
  File "dune", line 1, characters 17-33:
  1 | (enabled_if true (dirs :standard))
                       ^^^^^^^^^^^^^^^^
  Error: The "dirs" stanza cannot be used inside an enabled_if stanza.
  [1]

Conditions cannot depend on rules or libraries in the build.

  $ echo '(enabled_if %{bin-available:sh} (rule (write-file result sh)))' > dune
  $ dune build
  File "dune", line 1, characters 12-31:
  1 | (enabled_if %{bin-available:sh} (rule (write-file result sh)))
                  ^^^^^^^^^^^^^^^^^^^
  Error: Only %{profile} and %{context_name} are allowed in this stanza.
  [1]
