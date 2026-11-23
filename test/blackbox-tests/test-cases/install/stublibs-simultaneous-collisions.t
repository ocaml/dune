Two shared search directories can each contain the wrong DLL for a selected
library. Neither ordering of the directories can link the executable.
See #9979 and #16789.

Install an old a alongside the b that the executable will use.

  $ mkdir -p producer local
  $ write_stublibs_ab_producer
  $ touch producer/a.ml
  $ echo 'external value : unit -> int = "selected_b"' >producer/b.ml
  $ write_stublibs_primitive producer/a_stubs.c old_a 0
  $ write_stublibs_primitive producer/b_stubs.c selected_b 2
  $ install_stublibs_producer
  $ ext_dll=$(ocamlc -config-var ext_dll)

Build local a and an unrelated b in the same directory.

  $ make_dune_project 3.13
  $ echo '(data_only_dirs producer prefix)' >dune
  $ cat >local/dune <<'EOF'
  > (library
  >  (name a)
  >  (modules a)
  >  (foreign_stubs (language c) (names a_stubs)))
  > (library
  >  (name b)
  >  (modules b)
  >  (foreign_stubs (language c) (names b_stubs)))
  > EOF
  $ echo 'external value : unit -> int = "selected_a"' >local/a.ml
  $ touch local/b.ml
  $ write_stublibs_primitive local/a_stubs.c selected_a 1
  $ write_stublibs_primitive local/b_stubs.c unrelated_b 0
  $ dune build local/dllb_stubs$ext_dll
  $ echo 'let () = Printf.printf "%d %d\n" (A.value ()) (B.value ())' >main.ml

Installed directories first select the old a.

  $ cat >dune <<'EOF'
  > (data_only_dirs producer prefix)
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries pkg.b a))
  > EOF
  $ dune build main.bc
  File "_none_", line 1:
  Error: Error while linking .main.eobjs/byte/dune__exe__Main.cmo:
         The external function selected_a is not available
  [1]

Local directories first select the unrelated b.

  $ sed 's/(libraries pkg\.b a)/(libraries a pkg.b)/' dune > dune.new
  $ mv dune.new dune
  $ dune build main.bc
  File "_none_", line 1:
  Error: Error while linking .main.eobjs/byte/dune__exe__Main.cmo:
         The external function selected_b is not available
  [1]
