The explicit alias is harmless when there are no foreign stanzas.

  $ make_dune_project 3.23
  $ dune build @compile_commands
  $ test ! -f compile_commands.json
  $ test ! -f _build/default/.dune/compile_commands.json

Disabled stanzas must not require missing sources, replace an external database,
or register that database for deletion on clean.

  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (names missing)
  >  (enabled_if (= %{context_name} absent)))
  > EOF
  $ echo external > compile_commands.json
  $ dune build @check
  $ cat compile_commands.json
  external
  $ test ! -f _build/.to-delete-in-source-tree

Enabling foreign code generates the internal target and promotes the database.
An existing source-tree database must not suppress regeneration.

  $ echo 'int stub(void) { return 0; }' > stub.c
  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (names stub)
  >  (flags :standard -DFOO=1))
  > EOF
  $ dune build @check
  $ test -f _build/default/.dune/compile_commands.json
  $ jq '.[0].arguments | map(select(startswith("-DFOO")))' compile_commands.json
  [
    "-DFOO=1"
  ]
  $ test -f _build/.to-delete-in-source-tree

Deleting the promoted output must regenerate it.

  $ rm compile_commands.json
  $ dune build @check
  $ test -f compile_commands.json

Disabling everything leaves the last database alone.

  $ cp compile_commands.json previous.json
  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (names missing)
  >  (enabled_if false))
  > EOF
  $ dune build @check
  $ cmp previous.json compile_commands.json

Reenabling must refresh flags rather than treating the promoted file as owned
by the user.

  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (names stub)
  >  (flags :standard -DFOO=2))
  > EOF
  $ dune build @check
  $ jq '.[0].arguments | map(select(startswith("-DFOO")))' compile_commands.json
  [
    "-DFOO=2"
  ]

Predicted select outputs from disabled libraries are not user-owned targets.
They must not prevent the automatic database from refreshing.

  $ echo fallback > compile_commands.fallback.json
  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (names stub)
  >  (flags :standard -DFOO=3))
  > (library
  >  (name disabled)
  >  (modules)
  >  (enabled_if false)
  >  (libraries
  >   (select compile_commands.json from
  >    (-> compile_commands.fallback.json))))
  > EOF
  $ dune build @check
  $ jq '.[0].arguments | map(select(startswith("-DFOO")))' compile_commands.json
  [
    "-DFOO=3"
  ]

Enablement can query ordinary root targets, including generated files and
implicit executable targets, without building them just to collect metadata.

  $ mkdir availability && cd availability
  $ make_dune_project 3.23
  $ echo 'let () = ()' > main.ml
  $ cat > dune <<'EOF'
  > (executable (name main))
  > (rule
  >  (action (write-file generated.c "int generated(void) { return 0; }")))
  > EOF
  $ mkdir foo
  $ echo 'int stub(void) { return 0; }' > foo/stub.c
  $ cat > foo/dune <<'EOF'
  > (copy_files ../*.c)
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (enabled_if
  >   (and %{file-available:../generated.c} %{file-available:../main.exe})))
  > EOF
  $ dune build @compile_commands && jq '[.[].file] | sort' compile_commands.json
  [
    "generated.c",
    "stub.c"
  ]
  $ test ! -f _build/default/generated.c
  $ test ! -f _build/default/foo/generated.c
  $ test ! -f _build/default/main.exe

Nonrecursive lookup refreshes the database without searching subdirectories.

  $ rm -f compile_commands.json
  $ dune build @@compile_commands
  $ test -f compile_commands.json
  $ cd ..

An explicit user rule takes precedence, even when its promoted source file
already exists. The alias delegates to that rule, not the automatic generator.

  $ mkdir owned && cd owned
  $ make_dune_project 3.23
  $ echo 'int stub(void) { return 0; }' > stub.c
  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (names stub)
  >  (enabled_if false))
  > (rule
  >  (mode promote)
  >  (action (write-file compile_commands.json user)))
  > EOF
  $ dune build compile_commands.json
  $ cat compile_commands.json
  user
  $ cat > dune <<'EOF'
  > (foreign_library (archive_name stubs) (language c) (names stub))
  > (rule
  >  (mode promote)
  >  (action (write-file compile_commands.json updated)))
  > EOF
  $ dune build @check
  $ cat compile_commands.json
  updated
  $ test ! -f _build/default/.dune/compile_commands.json

Ownership checks also work at an include_subdirs group root.

  $ cat > dune <<'EOF'
  > (include_subdirs unqualified)
  > (foreign_library (archive_name stubs) (language c) (names stub))
  > (rule
  >  (mode promote)
  >  (action (write-file compile_commands.json grouped)))
  > EOF
  $ dune build @compile_commands
  $ cat compile_commands.json
  grouped
  $ test ! -f _build/default/.dune/compile_commands.json

Only the Merlin context produces and promotes the internal database.

  $ mkdir ../contexts && cd ../contexts
  $ make_dune_project 3.23
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.23)
  > (context (default))
  > (context (default (name editor) (merlin)))
  > EOF
  $ echo 'int stub(void) { return 0; }' > stub.c
  $ echo '(foreign_library (archive_name stubs) (language c) (names stub))' > dune
  $ dune build @compile_commands
  $ test -f _build/editor/.dune/compile_commands.json
  $ test ! -f _build/default/.dune/compile_commands.json
  $ jq '[.[].directory] | unique' compile_commands.json
  [
    "_build/editor"
  ]
