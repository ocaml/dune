Compiler and linker flags stay separate for mixed sources, in executables,
OCaml libraries, and foreign archives. Marker flags are inspected in rules,
not executed. C++ linker defaults are compared with the toolchain's own defaults
rather than assuming a particular runtime library.

Setup
=====

  $ make_dune_project 3.26
  $ echo '(use_standard_c_and_cxx_flags true)' >> dune-project
  $ mkdir exe lib archive
  $ touch exe/main.ml lib/bindings.ml
  $ for dir in exe lib archive; do
  >   touch "$dir/c.c" "$dir/cxx.cpp"
  > done
  $ cat > dune <<'EOF'
  > (env
  >  (_
  >   (c_flags :standard --test-env-c)
  >   (cxx_flags :standard --test-env-cxx)
  >   (link_flags :standard --test-env-link)))
  > EOF

Each case selects languages and either extends or replaces :standard. Dump its
rules without compiling or linking the empty source files.

  $ dump_rules () {
  >   local languages="$1" standard="$2"
  >   cat > exe/dune <<EOF
  > (executable
  >  (name main)
  >  (foreign_stubs
  >   (languages $languages)
  >   (flags $standard --test-stub-compile))
  >  (link_flags $standard --test-exe-link))
  > EOF
  >   cat > lib/dune <<EOF
  > (library
  >  (name bindings)
  >  (foreign_stubs
  >   (languages $languages)
  >   (flags $standard --test-stub-compile))
  >  (c_library_flags $standard --test-lib-link))
  > EOF
  >   cat > archive/dune <<EOF
  > (foreign_library
  >  (archive_name mixed)
  >  (languages $languages)
  >  (flags $standard --test-archive-compile)
  >  (c_library_flags $standard --test-archive-link))
  > EOF
  >   dune rules --format=json > rules.json
  > }

Keep only marker flags and linker defaults, omitting the library's own stubs
archive.

  $ linker_flags () {
  >   jq_dune -c '[.[] |
  >     (if any(ruleTargets[]; endswith("/main.exe")) then "exe"
  >      elif any(ruleTargets[]; endswith("/bindings.cma")) then "lib"
  >      elif any(ruleTargets[]; test("/dllmixed\\.")) then "archive"
  >      else empty end) as $kind |
  >     {kind: $kind,
  >      defaults: [ruleActionFlagValues(
  >        if $kind == "archive" then "-ldopt" else "-cclib" end) |
  >        select(. != "-lbindings_stubs" and
  >          (startswith("--test-") | not))],
  >      flags: [ruleActionRunArgv | .[] | select(startswith("--test-"))]}] |
  >     sort_by(.kind)[]' rules.json
  > }
  $ compiler_flags () {
  >   jq_dune -c '[.[] | ruleTargets as $targets |
  >     ruleActionRunArgv as $args |
  >     select($args[-1] | IN("c.c", "cxx.cpp")) |
  >     {dir: ($targets[0] | split("/")[-2]), source: $args[-1],
  >      flags: [$args[] | select(startswith("--test-"))]}] |
  >     sort_by(.dir, .source)[] | [.dir + "/" + .source, .flags]' rules.json
  > }

C-only control
==============

Explicit linker flags are retained, but no C++ runtime flags are added.

  $ dump_rules c :standard
  $ linker_flags
  {"kind":"archive","defaults":[],"flags":["--test-archive-link"]}
  {"kind":"exe","defaults":[],"flags":["--test-env-link","--test-exe-link"]}
  {"kind":"lib","defaults":[],"flags":["--test-lib-link"]}

Mixed-language defaults
=======================

Use C++ alone as the reference for linker defaults.

  $ dump_rules cxx :standard
  $ linker_flags > cxx.link

Adding C does not change linker flags. Each compiler gets only its own
environment defaults plus the shared stanza flags, with no linker flags.

  $ dump_rules 'c cxx' :standard
  $ linker_flags > mixed.link
  $ diff -u cxx.link mixed.link
  $ compiler_flags | tee mixed.compile
  ["archive/c.c",["--test-env-c","--test-archive-compile"]]
  ["archive/cxx.cpp",["--test-env-cxx","--test-archive-compile"]]
  ["exe/c.c",["--test-env-c","--test-stub-compile"]]
  ["exe/cxx.cpp",["--test-env-cxx","--test-stub-compile"]]
  ["lib/c.c",["--test-env-c","--test-stub-compile"]]
  ["lib/cxx.cpp",["--test-env-cxx","--test-stub-compile"]]

Reversing the languages changes neither compiler nor linker flags.

  $ dump_rules 'cxx c' :standard
  $ linker_flags > reversed.link
  $ diff -u mixed.link reversed.link
  $ compiler_flags > reversed.compile
  $ diff -u mixed.compile reversed.compile

Replacing defaults
==================

Omitting :standard removes both C++ linker defaults and inherited compiler
and linker environment flags, leaving only explicit stanza flags.

  $ dump_rules 'c cxx' ''
  $ linker_flags | tee override.link
  {"kind":"archive","defaults":[],"flags":["--test-archive-link"]}
  {"kind":"exe","defaults":[],"flags":["--test-exe-link"]}
  {"kind":"lib","defaults":[],"flags":["--test-lib-link"]}
  $ compiler_flags | tee override.compile
  ["archive/c.c",["--test-archive-compile"]]
  ["archive/cxx.cpp",["--test-archive-compile"]]
  ["exe/c.c",["--test-stub-compile"]]
  ["exe/cxx.cpp",["--test-stub-compile"]]
  ["lib/c.c",["--test-stub-compile"]]
  ["lib/cxx.cpp",["--test-stub-compile"]]

This is also independent of language order.

  $ dump_rules 'cxx c' ''
  $ linker_flags > reversed.link
  $ diff -u override.link reversed.link
  $ compiler_flags > reversed.compile
  $ diff -u override.compile reversed.compile

Single-language selections have the same replacement linker flags.

  $ for languages in c cxx; do
  >   dump_rules "$languages" ''
  >   linker_flags > single.link
  >   diff -u override.link single.link || exit 1
  > done
