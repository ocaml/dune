When we generate an opam formula that would require parentheses, they are
not present.
This is actually a bug in opam-file-format: ocaml/opam-file-format/issues#56.
See #3431.

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > 
  > (generate_opam_files)
  > 
  > (package
  >  (name p)
  >  (allow_empty)
  >  (depends
  >   (a
  >    (and
  >     (or :with-test :dev)
  >     (>= 1.1.0)))))
  > EOF

  $ dune build

  $ grep '"a"' p.opam
    "a" {with-test | dev & >= "1.1.0"}

(the correct line is: "a" {(with-test | dev) & >= "1.1.0"})
