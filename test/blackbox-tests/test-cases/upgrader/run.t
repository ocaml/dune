  $ dune upgrade
  Info: creating file dune-project with this contents:
  | (lang dune 1.0)
  | (name foo)
  
  Upgrading foo.opam...
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
     (run echo %{<}))))
  
  ; other
  ; comment
  
  (rule
   (copy x y))
  
  ;(sexp
  ;    comment)

  $ cat foo.opam
  build: [
    ["dune" "subst"]
    ["dune" "build" "-p" name "-j" jobs]
  ]
  depends: [
    "dune" {build & >= "1.0"}
  ]
