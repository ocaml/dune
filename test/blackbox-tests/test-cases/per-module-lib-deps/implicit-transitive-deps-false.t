Observational baseline: under [(implicit_transitive_deps false)],
the consumer [main] uses [intermediate_lib] which declares
[link_only_lib] as a transitive dep. [main]'s source never
references any module of [link_only_lib]; under
[(implicit_transitive_deps false)] (with [-H] support, mode
[Disabled_with_hidden_includes]), [link_only_lib] is on [main]'s
[-H] include path so the compiler can read its [.cmi] files via
alias chains, but [main] does not use any of them.

The mode-with-[-H] is only reached on dune lang [3.17+] with an
OCaml compiler that supports hidden includes (5.2+); older dune
lang versions fall back to mode [Disabled] without [-H]. This
test pins [(lang dune 3.23)] below to keep the [-H]-glob path the
one exercised — that is the path the future per-module filter is
expected to tighten.

On trunk today, [main]'s compile rule globs over
[link_only_lib]'s objdir as part of the cctx-wide [-H] glob, so
any [.cmi] content change in [link_only_lib] invalidates [main].
A tighter per-module filter could observe that no
[link_only_lib] entry name reaches [main]'s reference closure
and drop the lib from [main]'s compile-rule deps.

Reported by @nojb in
https://github.com/ocaml/dune/pull/14116#issuecomment-4323883194
and again, after a partial fix, in
https://github.com/ocaml/dune/pull/14116#issuecomment-4331209820.
Records [main]'s current rebuild count so a future per-module
filter can flip it to 0.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (implicit_transitive_deps false)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name link_only_lib)
  >  (wrapped false)
  >  (modules link_only_module))
  > (library
  >  (name intermediate_lib)
  >  (wrapped false)
  >  (modules intermediate_module)
  >  (libraries link_only_lib))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries intermediate_lib))
  > EOF

  $ cat > link_only_module.ml <<EOF
  > let x = 42
  > EOF

  $ cat > intermediate_module.ml <<EOF
  > let x = 42
  > EOF

  $ cat > main.ml <<EOF
  > let _ = Intermediate_module.x
  > EOF

  $ dune build ./main.exe

Empty [link_only_module.ml] so its [.cmi] loses the [val x : int]
binding. The cctx-wide glob over [link_only_lib]'s objdir fires
on the [.cmi] content change, invalidating [main] — both the
[.cmi]/[.cmti] rule and the [.cmx]/[.o] rule re-run:

  $ echo > link_only_module.ml
  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe__Main"))]'
  [
    {
      "target_files": [
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmi",
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmti"
      ]
    },
    {
      "target_files": [
        "_build/default/.main.eobjs/native/dune__exe__Main.cmx",
        "_build/default/.main.eobjs/native/dune__exe__Main.o"
      ]
    }
  ]
