Rule plugins require Dune language 3.26.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using plugin 0.1)
  > EOF
  $ dune build
  File "dune-project", line 2, characters 14-17:
  2 | (using plugin 0.1)
                    ^^^
  Error: Version 0.1 of rule plugin extension is not supported until version
  3.26 of the dune language.
  There are no supported versions of this extension in version 3.25 of the dune
  language.
  
  [1]

Invocations are not recognized in older language versions.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ echo '(plugin.missing)' > dune
  $ dune build
  File "dune", line 1, characters 1-15:
  1 | (plugin.missing)
       ^^^^^^^^^^^^^^
  Error: Unknown constructor plugin.missing
  [1]

Rule plugins must be enabled explicitly because the feature is experimental.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > EOF
  $ cat > dune <<'EOF'
  > (plugin
  >  (name generate)
  >  (executable ./rules.exe))
  > EOF

  $ dune build
  File "dune", lines 1-3, characters 0-51:
  1 | (plugin
  2 |  (name generate)
  3 |  (executable ./rules.exe))
  Error: 'plugin' is available only when plugin is enabled in the dune-project
  or workspace file. You must enable it using (using plugin 0.1) in the file.
  Note however that plugin is experimental and might change without notice in
  the future.
  [1]

Invocations are gated by the extension as well.

  $ cat > dune <<'EOF'
  > (plugin.missing)
  > EOF
  $ dune build
  File "dune", line 1, characters 0-16:
  1 | (plugin.missing)
      ^^^^^^^^^^^^^^^^
  Error: This syntax is available only when plugin is enabled in the
  dune-project or workspace file. You must enable it using (using plugin 0.1)
  in the file.
  Note however that plugin is experimental and might change without notice in
  the future.
  [1]

Once enabled, an invocation of an unknown plugin reports that it is missing.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ cat > dune <<'EOF'
  > (plugin.missing)
  > EOF

  $ dune build
  File "dune", line 1, characters 0-16:
  1 | (plugin.missing)
      ^^^^^^^^^^^^^^^^
  Error: Rule plugin "missing" is not defined in this project.
  [1]

Plugin definitions use local library-name validation.

  $ echo '(plugin (name "") (executable ./rules.exe))' > dune
  $ dune build > .#error-output 2>&1
  [1]
  $ grep '^Error:' .#error-output
  Error: "" is an invalid library name.
  $ echo '(plugin (name "bad name") (executable ./rules.exe))' > dune
  $ dune build > .#error-output 2>&1
  [1]
  $ grep '^Error:' .#error-output
  Error: "bad name" is an invalid library name.
  $ echo '(plugin (name bad-name) (executable ./rules.exe))' > dune
  $ dune build > .#error-output 2>&1
  [1]
  $ grep '^Error:' .#error-output
  Error: "bad-name" is an invalid library name.
  $ echo '(plugin (name public.name) (executable ./rules.exe))' > dune
  $ dune build > .#error-output 2>&1
  [1]
  $ grep '^Error:' .#error-output
  Error: "public.name" is an invalid library name.

Invalid invocation names produce ordinary user errors.

  $ echo '(plugin.bad-name)' > dune
  $ dune build > .#error-output 2>&1
  [1]
  $ grep '^Error:' .#error-output
  Error: "bad-name" is an invalid library name.
  $ echo '(plugin.public.name)' > dune
  $ dune build > .#error-output 2>&1
  [1]
  $ grep '^Error:' .#error-output
  Error: "public.name" is an invalid library name.
  $ echo '(plugin.)' > dune
  $ dune build > .#error-output 2>&1
  [1]
  $ grep '^Error:' .#error-output
  Error: "" is an invalid library name.

A valid local library name is accepted.

  $ echo '(plugin (name valid_name1) (executable ./rules.exe))' > dune
  $ dune build
  $ echo '(plugin (name 1name) (executable ./rules.exe))' > dune
  $ dune build
