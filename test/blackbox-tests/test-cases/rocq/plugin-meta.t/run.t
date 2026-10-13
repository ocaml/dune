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
  >          | length)})
  >   | sort_by(.target)'
  [
    {
      "target": "_build/default/other/qux.vo",
      "layout_files": 11
    },
    {
      "target": "_build/default/theory/bar.vo",
      "layout_files": 11
    },
    {
      "target": "_build/default/theory/baz.vo",
      "layout_files": 11
    }
  ]

  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules

  $ rm plugin/foo.ml
  $ echo 'let foo = "updated"' > plugin/foo.ml
  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules
  other/qux.v
  theory/bar.v
  theory/baz.v

  $ dune build theory/bar.vo theory/baz.vo other/qux.vo
  $ compiled_modules
