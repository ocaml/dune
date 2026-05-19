Regression guard for menhir mock-compile and sibling module
incremental rebuild when a sibling library's interface changes.

When a library declares a menhir grammar whose semantic action
references types or values from a `(libraries ...)` dep, dune emits
two compile rules whose deps cover that library's cmis: menhir's
`--infer` mock-compile (which runs `ocamlc -i` on a generated mock
module to type-check the grammar's semantic actions) and the regular
sibling-module compile that also references the dep. Both rules
must invalidate when the dep's interface changes.

Code under test: `src/dune_rules/menhir/menhir_rules.ml`.

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using menhir 3.0)
  > EOF

`dep` is a sibling library exposing a value the parent library's
menhir grammar will reference. `dep.ml` carries both `value` and
`extra` from the start so the `.mli` edit step is interface-only.

  $ make_dir_with_dune dep <<EOF
  > (library (name dep))
  > EOF
  $ cat > dep/dep.ml <<EOF
  > let value = 42
  > let extra = "x"
  > EOF
  $ cat > dep/dep.mli <<EOF
  > val value : int
  > EOF

`parser`: a library with a menhir grammar whose semantic action
references `Dep.value`, plus a sibling module `helper` that also
references `Dep.value`.

  $ make_dir_with_dune parser <<EOF
  > (library
  >  (name parser)
  >  (libraries dep))
  > (menhir (modules grammar))
  > EOF
  $ cat > parser/grammar.mly <<EOF
  > %token EOF
  > %start <int> main
  > %%
  > main: EOF { Dep.value }
  > EOF
  $ cat > parser/helper.ml <<EOF
  > let echo () = Dep.value
  > EOF
  $ cat > parser/helper.mli <<EOF
  > val echo : unit -> int
  > EOF

Initial build.

  $ dune build @check

Case 1: the mock-compile ran. The invocation is recognisable by
`-i` in its argv; its target is `grammar__mock.mli.inferred`. The
presence of this event positively asserts the `--infer` code path
ran:

  $ dune trace cat | jq -s 'include "dune"; [.[] | progMatching("ocamlc") | select(.process_args | index("-i") != null) | .target_files]'
  [
    [
      "_build/default/parser/grammar__mock.mli.inferred"
    ]
  ]

The mock's `-I` path includes `dep`'s objdir, so the grammar's
semantic action type-checks against `dep`:

  $ dune trace cat | jq -s 'include "dune"; [.[] | progMatching("ocamlc") | select(.process_args | index("-i") != null) | .process_args | [.[] | select(startswith("dep/"))]]'
  [
    [
      "dep/.dep.objs/byte"
    ]
  ]

The mock-compile rule's declared dep set (no `%{...}` variable
resolves the menhir-generated mock target, hence the literal path):

  $ dune rules --root . --format=json --deps _build/default/parser/grammar__mock.mli.inferred |
  > jq -r 'include "dune"; .[] | depsGlobs | "\(.dir_kind) \(.dir) \(.predicate)"'
  In_build_dir _build/default/dep/.dep.objs/byte *.cmi

Case 2: editing `dep`'s `.mli` to expose `extra` must invalidate
both consumers — the menhir mock-compile and `helper`'s `.cmo`.
The `.ml` already carries `extra`, so this is an interface-only
edit.

  $ cat > dep/dep.mli <<EOF
  > val value : int
  > val extra : string
  > EOF

Second build. Assert independently that each of the two
invariants holds: the menhir mock-compile rebuilt, and the helper
`.cmo` rebuilt. The test stays robust whether other helper
artefacts (cmi/cmti) also rebuild — that is governed by
cctx-wide-vs-per-module dep filtering and isn't the property
under guard here.

  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("grammar__mock\\.mli\\.inferred"))]'
  [
    {
      "target_files": [
        "_build/default/parser/grammar__mock.mli.inferred"
      ]
    }
  ]
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("/parser__Helper\\.cmo$"))]'
  [
    {
      "target_files": [
        "_build/default/parser/.parser.objs/byte/parser__Helper.cmo",
        "_build/default/parser/.parser.objs/byte/parser__Helper.cmt"
      ]
    }
  ]
