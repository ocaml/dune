  $ dune upgrade
  Info: Creating file dune-project with this contents:
  | (lang dune 1.0)
  | (name foo)
  File "jbuild", line 1, characters 0-0:
  Warning: jbuild files are not allowed inside Dune 2.0 projects, please
  convert this file to a dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  Upgrading foo.opam...
  Upgrading jbuild.inc to dune.inc...
  Upgrading jbuild to dune...

  $ cat dune
  ;old style
  ;block comment
  ;
  
  (rule
   (deps x y z) ; abc
   (targets z)
   ; def
   (action
    (with-stdout-to
     z
     (run echo ${<})))
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
   (deps a)
   (targets b)
   (action
    (copy ${<} ${@})))

  $ cat foo.opam
  build: [
    ["dune" "subst"]
    ["dune" "build" "-p" name "-j" jobs]
  ]
  depends: [
    "dune" {build & >= "1.0"}
  ]
