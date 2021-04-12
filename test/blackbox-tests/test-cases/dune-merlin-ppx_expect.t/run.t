  $ dune build

Expected behavior with Merlin >= 4.2
  $ ocamlmerlin single errors -filename lib/sublib/bar.ml < lib/sublib/bar.ml
  {"class":"return","value":[],"notifications":[],"timing":{"clock":54,"cpu":37,"query":2,"pp":0,"reader":4,"ppx":20,"typer":11,"error":0}}
