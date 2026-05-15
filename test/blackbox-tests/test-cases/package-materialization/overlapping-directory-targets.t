Regression test for #13307: two packages installing directory targets
to share_root collide. This produces an internal error because the
install context cannot handle overlapping directory targets from
different packages in the same _root section.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name bin1))
  > (package (name bin2))
  > EOF

  $ mkdir -p bin1-src/share bin2-src/share lib-src

  $ cat >lib-src/dune <<EOF
  > (library (name installme))
  > EOF
  $ cat >lib-src/installme.ml <<EOF
  > let x = 1
  > EOF

  $ cat >bin1-src/dune <<EOF
  > (executable (public_name bin1) (package bin1) (name main) (libraries installme))
  > (install (section share_root) (package bin1) (dirs (share as .)))
  > EOF
  $ cat >bin1-src/main.ml <<EOF
  > let () = print_endline "bin1"
  > EOF
  $ echo "bin1 data" > bin1-src/share/data1.txt

  $ cat >bin2-src/dune <<EOF
  > (executable (public_name bin2) (package bin2) (name main) (libraries installme))
  > (install (section share_root) (package bin2) (dirs (share as .)))
  > EOF
  $ cat >bin2-src/main.ml <<EOF
  > let () = print_endline "bin2"
  > EOF
  $ echo "bin2 data" > bin2-src/share/data2.txt

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package bin1) (package bin2))
  >  (action (with-stdout-to out (echo "ok"))))
  > EOF

  $ dune build out 2>&1 | head -5
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Map.add_exn: key already exists", { key = "install/default/share" })
  [1]
