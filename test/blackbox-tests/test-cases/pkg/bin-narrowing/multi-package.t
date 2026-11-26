Per-owner binary visibility is independent of the provider's backend. Each
owner sees its declared tool, not the other owner's tool; an owner with no
dependencies sees neither. Exercise workspace and mounted requesters against
the same native/opaque providers.

Create two distinct tools with renamed installation entries and shared
requester rules. Lookup must use the installed name, not the source basename.

  $ mkdir -p _providers _requesters native opaque
  $ (set -eu
  >  for owner in a b; do
  >    source="_providers/tool-$owner"
  >    mkdir -p "$source" "_requesters/$owner"
  >    printf '(lang dune 3.25)\n(package (name tool-%s))\n' "$owner" \
  >      > "$source/dune-project"
  >    printf '(install (section bin) (files (tool-%s.sh as bin-%s)))\n' \
  >      "$owner" "$owner" > "$source/dune"
  >    printf '#!/bin/sh\necho from-%s\n' "$owner" > "$source/tool-$owner.sh"
  >    chmod +x "$source/tool-$owner.sh"
  >    cat > "_requesters/$owner/dune" <<EOF
  > (rule
  >  (target available)
  >  (action
  >   (with-stdout-to %{target}
  >    (echo %{bin-available:bin-a} %{bin-available:bin-b}))))
  > (rule
  >  (target pform-output)
  >  (action (with-stdout-to %{target} (run %{bin:bin-$owner}))))
  > (rule
  >  (target literal-output)
  >  (action (with-stdout-to %{target} (run bin-$owner))))
  > (rule
  >  (target path)
  >  (action (with-stdout-to %{target} (bash "echo \$PATH"))))
  > EOF
  >  done
  >  mkdir _requesters/denied
  >  cp _requesters/a/dune _requesters/denied/dune
  > )
  $ cat > _requesters/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name pkg-a) (allow_empty) (dir a) (depends tool-a))
  > (package (name pkg-b) (allow_empty) (dir b) (depends tool-b))
  > (package (name denied) (allow_empty) (dir denied))
  > EOF
  $ mkdir _mounted
  $ cp -R _requesters/. _mounted/
  $ sed -i 's/(name \(pkg-\)*/(name mounted-/' _mounted/dune-project
  $ make_case() (
  >   set -eu
  >   local backend="$1"
  >   cp -R _requesters/. "$backend/"
  >   cd "$backend"
  >   make_lockdir
  >   for owner in a b; do
  >     make_dune_source_lockpkg "$backend" "tool-$owner" \
  >       "$PWD/../_providers/tool-$owner"
  >     make_dune_source_lockpkg native "mounted-$owner" "$PWD/../_mounted"
  >     printf '(depends tool-%s)\n' "$owner" \
  >       | append_to_lockpkg "mounted-$owner"
  >   done
  >   make_dune_source_lockpkg native mounted-denied "$PWD/../_mounted"
  > )
  $ make_case native
  $ make_case opaque

Each owner check builds the three outputs of one requester directory and
prints them. A denied owner builds each output separately, so the pform and
literal lookups fail on their own.

  $ show_owner() (
  >   cd "$1"
  >   dune build "$2"/{available,pform-output,literal-output}
  >   echo "$(cat "$2/available")"
  >   cat "$2"/{pform-output,literal-output}
  > )
  $ show_denied() (
  >   cd "$1"
  >   dune build "$2/available"
  >   echo "$(cat "$2/available")"
  >   dune build "$2/pform-output"
  >   dune build "$2/literal-output"
  > )

First build only owner a. Looking up its declared tool must not materialise
the unrelated tool: inspect the native artifact, not its unused install prefix.

  $ show_owner native _build/default/a
  true false
  from-a
  from-a
  $ test -f native/_build/default/.pkg/tool-a/tool-a.sh
  $ test ! -e native/_build/default/.pkg/tool-b/tool-b.sh
  $ test ! -e native/_build/default/.pkg/tool-a/_install/cookie

The opaque case has the same cold lookup, but cookie existence is the
load-bearing build check for this backend.

  $ show_owner opaque _build/default/a
  true false
  from-a
  from-a
  $ test ! -e opaque/_build/default/.pkg/tool-b/_install/cookie

The reciprocal owner sees b, not a. Both providers are now built, so later
denials cannot pass just because their program has not been built yet.

  $ show_owner native _build/default/b
  false true
  from-b
  from-b
  $ show_owner opaque _build/default/b
  false true
  from-b
  from-b
  $ cat opaque/_build/default/.pkg/tool-{a,b}/_install/opaque-recipe
  opaque
  opaque
  $ test -f opaque/_build/default/.pkg/tool-a/_install/cookie
  $ test -f opaque/_build/default/.pkg/tool-b/_install/cookie
  $ test ! -e native/_build/default/.pkg/tool-b/_install/cookie

Declaring a provider in the package's dependencies does not put it on the
PATH of the package's actions; an action gets it by depending on the package.

  $ cd opaque
  $ dune build a/path b/path
  $ env_added "$(cat _build/default/a/path)" "$PATH" | censor
  
  [1]
  $ env_added "$(cat _build/default/b/path)" "$PATH" | censor
  
  [1]
  $ cd ..

Mounted requesters use their lock metadata, not the workspace owner's
dependencies. Both reciprocal owners must still resolve their own tool.

  $ mounted_a=$pkg_root/mounted-a/a
  $ mounted_b=$pkg_root/mounted-b/b
  $ mounted_denied=$pkg_root/mounted-denied/denied
  $ show_owner native $mounted_a
  true false
  from-a
  from-a
  $ show_owner native $mounted_b
  false true
  from-b
  from-b
  $ show_owner opaque $mounted_a
  true false
  from-a
  from-a
  $ show_owner opaque $mounted_b
  false true
  from-b
  from-b

An empty-dependency owner cannot discover or run either warmed provider.
Keep the failing pform and literal lookups separate, including the logical
mounted paths in their diagnostics.

  $ show_denied native _build/default/denied
  false false
  File "denied/dune", line 8, characters 40-52:
  8 |  (action (with-stdout-to %{target} (run %{bin:bin-a}))))
                                              ^^^^^^^^^^^^
  Error: Program bin-a not found in the tree or in PATH
   (context: default)
  Hint: add a dependency on the package installing "bin-a" to this package
  File "denied/dune", line 11, characters 40-45:
  11 |  (action (with-stdout-to %{target} (run bin-a))))
                                               ^^^^^
  Error: Program bin-a not found in the tree or in PATH
   (context: default)
  Hint: add a dependency on the package installing "bin-a" to this package
  [1]
  $ show_denied native $mounted_denied
  false false
  File "_build/default/.pkg/mounted-denied/denied/dune", line 8, characters 40-52:
  Error: Program bin-a not found in the tree or in PATH
   (context: default)
  Hint: add a dependency on the package installing "bin-a" to this package
  File "_build/default/.pkg/mounted-denied/denied/dune", line 11, characters 40-45:
  Error: Program bin-a not found in the tree or in PATH
   (context: default)
  Hint: add a dependency on the package installing "bin-a" to this package
  [1]
  $ show_denied opaque _build/default/denied
  false false
  File "denied/dune", line 8, characters 40-52:
  8 |  (action (with-stdout-to %{target} (run %{bin:bin-a}))))
                                              ^^^^^^^^^^^^
  Error: Program bin-a not found in the tree or in PATH
   (context: default)
  Hint: add a dependency on the package installing "bin-a" to this package
  File "denied/dune", line 11, characters 40-45:
  11 |  (action (with-stdout-to %{target} (run bin-a))))
                                               ^^^^^
  Error: Program bin-a not found in the tree or in PATH
   (context: default)
  Hint: add a dependency on the package installing "bin-a" to this package
  [1]
  $ show_denied opaque $mounted_denied
  false false
  File "_build/default/.pkg/mounted-denied/denied/dune", line 8, characters 40-52:
  Error: Program bin-a not found in the tree or in PATH
   (context: default)
  Hint: add a dependency on the package installing "bin-a" to this package
  File "_build/default/.pkg/mounted-denied/denied/dune", line 11, characters 40-45:
  Error: Program bin-a not found in the tree or in PATH
   (context: default)
  Hint: add a dependency on the package installing "bin-a" to this package
  [1]
