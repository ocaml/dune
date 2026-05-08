Precision regression guard for the per-pair tight-eligibility
in [Lib_index]: when a consumer references only the [Some]-
entry modules of a mixed-pp lib, the [None]-entry modules must
NOT be pulled into the consumer's compile-rule deps.

We assert precision by giving [b] (the [None]-entry module) an
unresolvable identifier and leaving [a]'s source independent of
[b]. If the consumer's compile rule listed [b.cmi] as a dep,
dune would try to compile [b.ml] and fail. Building the
consumer's [.cmo] (compile only, no link) succeeds → [b] is
correctly excluded.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

A no-op staged ppx (identical to the soundness reproducer).

  $ mkdir ppx
  $ cat > ppx/dune <<EOF
  > (library
  >  (name ppx_noop)
  >  (kind ppx_rewriter)
  >  (ppx.driver (main Ppx_noop.main)))
  > EOF
  $ cat > ppx/ppx_noop.ml <<EOF
  > let main () =
  >   let n = Array.length Sys.argv in
  >   if n < 2 then assert false;
  >   let input = Sys.argv.(n - 2) in
  >   let output = Sys.argv.(n - 1) in
  >   Filename.quote_command "cp" [input; output]
  >   |> Sys.command
  >   |> exit
  > EOF

[mylib]: [a] uses default (Some entry), [b] uses staged_pps
(None entry). [a]'s source is independent of [b]; [b.ml]
contains an unresolvable identifier so any attempt to compile
it will fail.

  $ mkdir mylib
  $ cat > mylib/dune <<EOF
  > (library
  >  (name mylib)
  >  (wrapped false)
  >  (preprocess (per_module ((staged_pps ppx_noop) b))))
  > EOF
  $ cat > mylib/a.ml <<EOF
  > let answer = 42
  > EOF
  $ cat > mylib/b.ml <<EOF
  > let bar = no_such_thing
  > EOF

[consumer] references only [A]:

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries mylib))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let () = print_int A.answer
  > EOF

Build only the consumer's [.cmo] (compile rule, not link). If
per-pair tight-eligibility holds, the consumer depends on
[a.cmi] alone and the build succeeds. If the fix regressed and
[mylib] got globbed for the consumer, dune would attempt to
compile [b.ml] and fail.

  $ dune build consumer/.consumer.eobjs/byte/dune__exe__Consumer.cmo
