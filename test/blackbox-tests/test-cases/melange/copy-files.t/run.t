Test copy_files

For the executable copy_files work fine

  $ dune build native.exe
  $ dune exec ./native.exe
  Hello world

But for melange it fails

  $ dune build output/mel.js
  File "mel.ml", line 1, characters 26-31:
  1 | print_endline ("Hello " ^ Bar.x)
                                ^^^^^
  Error: Unbound module Bar
  File "output/bar/_unknown_", line 1, characters 0-0:
  Error: No rule found for bar/.bar.objs/melange/bar.cmj
  File "output/bar/_unknown_", line 1, characters 0-0:
  Error: No rule found for bar/.bar.objs/melange/bar__.cmj
  File "output/bar/_unknown_", line 1, characters 0-0:
  Error: No rule found for bar/.bar.objs/melange/bar__Baz.cmj
  [1]
