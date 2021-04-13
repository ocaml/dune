  $ dune build

Expected behavior with Merlin >= 4.2
  $ ocamlmerlin single errors -filename lib/foo.ml < lib/foo.ml
  {"class":"return","value":[{"start":{"line":0,"col":-1},"end":{"line":0,"col":-1},"type":"typer","sub":[],"valid":true,"message":"I/O error: foo.ml: No such file or\ndirectory"}],"notifications":[],"timing":{"clock":49,"cpu":31,"query":1,"pp":0,"reader":4,"ppx":22,"typer":4,"error":0}}
