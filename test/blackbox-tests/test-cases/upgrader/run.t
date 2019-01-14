  $ dune upgrade
  Info: creating file dune-project with this contents:
  | (lang dune 1.0)
  | (name foo)
  
  Upgrading foo.opam...
  Upgrading jbuild to dune...

  $ cat dune
  (rule
   (deps
    (:< x)
    y
    z)
   (targets z)
   (action
    (with-stdout-to
     z
     (run echo %{<}))))
  
  (rule
   (copy x y))

  $ cat foo.opam
  build: [
    ["dune" "subst"]
    ["dune" "build" "-p" name "-j" jobs]
  ]
  depends: [
    "dune" {build & >= "1.0"}
  ]
