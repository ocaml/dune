The package layout for plugins is materialized before calling rocqdep. Both
the META file and native plugin are present:
  $ mkdir plugin theory
  $ cp foo.ml plugin/foo.ml
  $ cat > plugin/dune << EOF
  > (library
  >  (public_name bar.foo)
  >  (name foo))
  > EOF
  $ cat > theory/dune << EOF
  > (rocq.theory
  >  (name bar)
  >  (plugins bar.foo))
  > EOF

  $ echo 'Declare ML Module "bar.foo".' > theory/bar.v
  $ dune build theory/.bar.theory.d
  $ find _build/install/default/.packages \( -name META -o -name '*.cmxs' \) \
  >   | sort | censor
  _build/install/default/.packages/$DIGEST/lib/bar/META
  _build/install/default/.packages/$DIGEST/lib/bar/foo/foo.cmxs

The plugin is used by theory/bar.v and, transitively, by other/qux.v.
theory/baz.v does not load it. Currently all three proofs depend on the complete
plugin layout.

  $ touch theory/baz.v
  $ mkdir other
  $ cat > other/dune << EOF
  > (rocq.theory
  >  (name qux)
  >  (theories bar))
  > EOF
  $ echo 'From bar Require Import bar.' > other/qux.v
  $ compiled_modules() {
  >   dune trace cat | jq -r '
  >     select(.cat == "process" and .name == "finish")
  >     | .args.process_args
  >     | select(.[0] == "compile")
  >     | .[] | select(endswith(".v"))' | sort
  > }

  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules
  other/qux.v
  theory/bar.v
  theory/baz.v

  $ dune rules --format=json theory/bar.vo theory/baz.vo other/qux.vo | jq '
  >   include "dune";
  >   map({target: ([ruleTargets[] | select(endswith(".vo"))][0]),
  >        layout_files: ([ruleDepFilePathsOfKind("In_build_dir")
  >          | select(startswith("_build/install/default/.packages/"))]
  >          | length),
  >        layout_groups: ([ruleDepGlobEntries
  >          | select(.dir | startswith("_build/install/default/.packages/"))]
  >          | length)})
  >   | sort_by(.target)'
  [
    {
      "target": "_build/default/other/qux.vo",
      "layout_files": 0,
      "layout_groups": 2
    },
    {
      "target": "_build/default/theory/bar.vo",
      "layout_files": 2,
      "layout_groups": 2
    },
    {
      "target": "_build/default/theory/baz.vo",
      "layout_files": 0,
      "layout_groups": 2
    }
  ]

  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules

The two dependency-discovery rules use the same plugin package set. They share
file groups rather than repeating the complete layout dependencies:

  $ dune rules --format=json theory/.bar.theory.d other/.qux.theory.d | jq '
  >   include "dune";
  >   map([ruleDepGlobEntries
  >        | select(.dir | startswith("_build/install/default/.packages/"))])
  >   as $groups
  >   | {layout_file_dependencies:
  >      ([.[] | ruleDepFilePathsOfKind("In_build_dir")
  >        | select(startswith("_build/install/default/.packages/"))]
  >       | length),
  >      shared_layout_groups:
  >        ($groups[0] == $groups[1] and ($groups[0] | length) > 0)}'
  {
    "layout_file_dependencies": 0,
    "shared_layout_groups": true
  }

  $ rm plugin/foo.ml
  $ echo 'let foo = "updated"' > plugin/foo.ml
  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules
  other/qux.v
  theory/bar.v
  theory/baz.v

  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules

Discovery's layout files must actually be inside the sandbox, even with alias
expansion disabled. Checking only whether rocqdep succeeds is insufficient:
its absolute OCAMLPATH can access the original, unsandboxed layout.
A root-section entry beside META must remain excluded to avoid a build cycle.

  $ echo library > 'payload[1].dat'
  $ echo excluded > excluded.txt
  $ cat > dune << EOF
  > (install
  >  (section lib)
  >  (files (glob_files payload*.dat)))
  > (install
  >  (section lib_root)
  >  (files
  >   (theory/bar.vo as bar/proof.vo)
  >   (excluded.txt as bar/payload1.dat)))
  > EOF

The mixed directory groups its library files using literal-set membership
rather than a linear name-list filter. All-library directories still use
unconditional selectors.

  $ dune rules --format=json theory/.bar.theory.d | jq '
  >   include "dune";
  >   .[0]
  >   | [ruleDepGlobEntries
  >      | select(.dir | startswith("_build/install/default/.packages/"))]
  >   as $groups
  >   | {layout_files:
  >        ([ruleDepFilePathsOfKind("In_build_dir")
  >          | select(startswith("_build/install/default/.packages/"))]
  >         | length),
  >      layout_groups: ($groups | length),
  >      unconditional_groups: all($groups[]; .predicate == ["or", []])}'
  {
    "layout_files": 0,
    "layout_groups": 2,
    "unconditional_groups": false
  }

  $ export REAL_ROCQ=$(command -v rocq)
  $ mkdir bin
  $ cat > bin/rocq <<'EOF'
  > #!/bin/sh
  > if [ "$1" = dep ]; then
  >   case "$PWD" in
  >     */_sandbox/.sandbox/*/default/theory) ;;
  >     *) echo 'Discovery is not sandboxed' >&2; exit 1 ;;
  >   esac
  >   for file in ../../install/default/.packages/*/lib/bar/META \
  >       ../../install/default/.packages/*/lib/bar/foo/foo.cmxs \
  >       ../../install/default/.packages/*/lib/bar/'payload[1].dat'; do
  >     if [ ! -f "$file" ]; then
  >       echo 'Missing sandbox plugin file' >&2
  >       exit 1
  >     fi
  >   done
  >   for file in ../../install/default/.packages/*/lib/bar/proof.vo \
  >       ../../install/default/.packages/*/lib/bar/payload1.dat; do
  >     if [ -e "$file" ]; then
  >       echo 'Unexpected root-section sandbox file' >&2
  >       exit 1
  >     fi
  >   done
  > fi
  > exec "$REAL_ROCQ" "$@"
  > EOF
  $ chmod +x bin/rocq
  $ PATH="$PWD/bin:$PATH" dune build --build-dir _sandbox --sandbox=copy \
  >   theory/.bar.theory.d

Library contents and membership remain dependencies of every proof. The
root-section file whose name matches the bracketed filename as a glob does not.

  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules
  other/qux.v
  theory/bar.v
  theory/baz.v

  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules

  $ echo changed > 'payload[1].dat'
  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules
  other/qux.v
  theory/bar.v
  theory/baz.v

  $ echo changed > excluded.txt
  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules

  $ echo added > payload-added.dat
  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules
  other/qux.v
  theory/bar.v
  theory/baz.v

  $ rm payload-added.dat
  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules
  other/qux.v
  theory/bar.v
  theory/baz.v

  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules
