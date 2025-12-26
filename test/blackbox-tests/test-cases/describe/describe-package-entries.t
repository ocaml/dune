Test for the `dune describe package-entries` command

  $ cat >dune-project <<EOF
  > (lang dune 3.10)
  > (package
  >  (name foo)
  >  (synopsis "describe package-entries"))
  > (generate_opam_files)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (public_name foo)
  >  (modules foo))
  > 
  > (executable
  >  (name main)
  >  (libraries foo)
  >  (modules main))
  > 
  > (install
  >  (section bin)
  >  (package foo)
  >  (files main.exe))
  > EOF

  $ touch main.ml
  $ touch foo.ml
  $ touch foo.opam

  $ dune describe package-entries
  ((foo
    (((source Dune)
      (entry
       ((src
         (In_build_dir default/META.foo))
        (kind File)
        (dst META)
        (section LIB)
        (optional false))))
     ((source Dune)
      (entry
       ((src
         (In_build_dir default/foo.dune-package))
        (kind File)
        (dst dune-package)
        (section LIB)
        (optional false))))
     ((source
       (User
        ((pos_fname dune)
         (start
          ((pos_lnum 1)
           (pos_bol 0)
           (pos_cnum 0)))
         (stop
          ((pos_lnum 3)
           (pos_bol 28)
           (pos_cnum 43))))))
      (entry
       ((src
         (In_build_dir default/foo.a))
        (kind File)
        (dst foo.a)
        (section LIB)
        (optional false))))
     ((source
       (User
        ((pos_fname dune)
         (start
          ((pos_lnum 1)
           (pos_bol 0)
           (pos_cnum 0)))
         (stop
          ((pos_lnum 3)
           (pos_bol 28)
           (pos_cnum 43))))))
      (entry
       ((src
         (In_build_dir default/foo.cma))
        (kind File)
        (dst foo.cma)
        (section LIB)
        (optional false))))
     ((source
       (User
        ((pos_fname dune)
         (start
          ((pos_lnum 1)
           (pos_bol 0)
           (pos_cnum 0)))
         (stop
          ((pos_lnum 3)
           (pos_bol 28)
           (pos_cnum 43))))))
      (entry
       ((src
         (In_build_dir default/.foo.objs/byte/foo.cmi))
        (kind File)
        (dst foo.cmi)
        (section LIB)
        (optional false))))
     ((source
       (User
        ((pos_fname dune)
         (start
          ((pos_lnum 1)
           (pos_bol 0)
           (pos_cnum 0)))
         (stop
          ((pos_lnum 3)
           (pos_bol 28)
           (pos_cnum 43))))))
      (entry
       ((src
         (In_build_dir default/.foo.objs/byte/foo.cmt))
        (kind File)
        (dst foo.cmt)
        (section LIB)
        (optional false))))
     ((source
       (User
        ((pos_fname dune)
         (start
          ((pos_lnum 1)
           (pos_bol 0)
           (pos_cnum 0)))
         (stop
          ((pos_lnum 3)
           (pos_bol 28)
           (pos_cnum 43))))))
      (entry
       ((src
         (In_build_dir default/.foo.objs/native/foo.cmx))
        (kind File)
        (dst foo.cmx)
        (section LIB)
        (optional false))))
     ((source
       (User
        ((pos_fname dune)
         (start
          ((pos_lnum 1)
           (pos_bol 0)
           (pos_cnum 0)))
         (stop
          ((pos_lnum 3)
           (pos_bol 28)
           (pos_cnum 43))))))
      (entry
       ((src
         (In_build_dir default/foo.cmxa))
        (kind File)
        (dst foo.cmxa)
        (section LIB)
        (optional false))))
     ((source
       (User
        ((pos_fname dune)
         (start
          ((pos_lnum 1)
           (pos_bol 0)
           (pos_cnum 0)))
         (stop
          ((pos_lnum 3)
           (pos_bol 28)
           (pos_cnum 43))))))
      (entry
       ((src
         (In_build_dir default/foo.ml))
        (kind File)
        (dst foo.ml)
        (section LIB)
        (optional false))))
     ((source Dune)
      (entry
       ((src
         (In_build_dir default/foo.opam))
        (kind File)
        (dst opam)
        (section LIB)
        (optional false))))
     ((source
       (User
        ((pos_fname dune)
         (start
          ((pos_lnum 1)
           (pos_bol 0)
           (pos_cnum 0)))
         (stop
          ((pos_lnum 3)
           (pos_bol 28)
           (pos_cnum 43))))))
      (entry
       ((src
         (In_build_dir default/foo.cmxs))
        (kind File)
        (dst foo.cmxs)
        (section LIBEXEC)
        (optional false))))
     ((source
       (User
        ((pos_fname dune)
         (start
          ((pos_lnum 13)
           (pos_bol 144)
           (pos_cnum 152)))
         (stop
          ((pos_lnum 13)
           (pos_bol 144)
           (pos_cnum 160))))))
      (entry
       ((src
         (In_build_dir default/main.exe))
        (kind File)
        (dst main.exe)
        (section BIN)
        (optional false)))))))
