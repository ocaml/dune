  $ dune upgrade
  Info: Creating file dune-project with this contents:
  | (lang dune 1.0)
  | (name foo)
  Upgrading foo.opam...
  Upgrading jbuild.inc to dune.inc...
  Upgrading jbuild to dune...

  $ cat dune
  ;old style
  ;block comment
  ;
  
  (rule
   (deps
    (:< x)
    y
    z) ; abc
   (targets z)
   ; def
   (action
    (with-stdout-to
     z
     (run echo %{<})))
   (mode fallback))
  
  ; other
  ; comment
  
  (rule
   (copy x y))
  
  ;(sexp
  ;    comment)
  
  (include dune.inc)

  $ cat dune.inc
  (rule
   (deps
    (:< a))
   (targets b)
   (action
    (copy %{<} %{targets})))

  $ cat foo.opam
  build: [
    ["dune" "subst"]
    ["dune" "build" "-p" name "-j" jobs]
  ]
  depends: [
    "dune" {build & >= "1.0"}
  ]
