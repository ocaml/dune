Jsoo and build-info

  $ echo "(lang dune 3.17)" > dune-project
  $ dune build
  Warning: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.wasm.js
  unknown
  $ dune install --prefix _install --display short 2>&1 | sed 's/-[a-f0-9]*[.]wasm/.wasm/'
  Installing _install/lib/main/META
  Installing _install/lib/main/dune-package
  Installing _install/lib/main/opam
  Installing _install/bin/main
  Installing _install/bin/main.bc.wasm.assets/Build_info.wasm
  Installing _install/bin/main.bc.wasm.assets/Build_info.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Build_info__Build_info_data.wasm
  Installing _install/bin/main.bc.wasm.assets/Build_info__Build_info_data.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalAtomic.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalAtomic.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormat.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormat.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormatBasics.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormatBasics.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalLazy.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalLazy.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Dune__exe__Main.wasm
  Installing _install/bin/main.bc.wasm.assets/Dune__exe__Main.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Std_exit.wasm
  Installing _install/bin/main.bc.wasm.assets/Std_exit.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Buffer.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Buffer.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Bytes.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Bytes.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Char.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Char.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Int.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Int.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Lazy.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Lazy.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__List.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__List.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Obj.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Obj.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Printf.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Printf.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Seq.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Seq.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__String.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__String.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Sys.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Sys.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Uchar.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Uchar.wasm.map
  Installing _install/bin/main.bc.wasm.assets/prelude.wasm
  Installing _install/bin/main.bc.wasm.assets/runtime.wasm
  Installing _install/bin/main.bc.wasm.assets/start.wasm
  Installing _install/bin/main.bc.wasm.assets/start-291802e1.wat
  Installing _install/bin/main.bc.wasm.js
  $ node _install/bin/main.bc.wasm.js
  unknown
  $ git init -q
  $ touch README
  $ git add README
  $ git commit -m "initial" -q
  $ git tag v1 -am "V1"
  $ git commit -m "empty2" --allow-empty -q
  $ echo "HELLO" > README
  $ dune build
  Warning: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.wasm.js
  unknown
  $ dune install --prefix _install --display short 2>&1 | sed 's/-[a-f0-9]*[.]wasm/.wasm/'
  Deleting _install/lib/main/META
  Installing _install/lib/main/META
  Deleting _install/lib/main/dune-package
  Installing _install/lib/main/dune-package
  Deleting _install/lib/main/opam
  Installing _install/lib/main/opam
  Deleting _install/bin/main
  Installing _install/bin/main
  Deleting _install/bin/main.bc.wasm.assets/Build_info.wasm
  Installing _install/bin/main.bc.wasm.assets/Build_info.wasm
  Deleting _install/bin/main.bc.wasm.assets/Build_info.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Build_info.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Build_info__Build_info_data.wasm
  Deleting _install/bin/main.bc.wasm.assets/Build_info__Build_info_data.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Build_info__Build_info_data.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalAtomic.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalAtomic.wasm
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalAtomic.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalAtomic.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalFormat.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormat.wasm
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalFormat.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormat.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalFormatBasics.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormatBasics.wasm
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalFormatBasics.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormatBasics.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalLazy.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalLazy.wasm
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalLazy.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalLazy.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Dune__exe__Main.wasm
  Installing _install/bin/main.bc.wasm.assets/Dune__exe__Main.wasm
  Deleting _install/bin/main.bc.wasm.assets/Dune__exe__Main.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Dune__exe__Main.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Std_exit.wasm
  Installing _install/bin/main.bc.wasm.assets/Std_exit.wasm
  Deleting _install/bin/main.bc.wasm.assets/Std_exit.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Std_exit.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Buffer.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Buffer.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Buffer.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Buffer.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Bytes.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Bytes.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Bytes.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Bytes.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Char.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Char.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Char.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Char.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Int.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Int.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Int.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Int.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Lazy.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Lazy.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Lazy.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Lazy.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__List.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__List.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__List.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__List.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Obj.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Obj.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Obj.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Obj.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Printf.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Printf.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Printf.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Printf.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Seq.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Seq.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Seq.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Seq.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__String.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__String.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__String.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__String.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Sys.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Sys.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Sys.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Sys.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Uchar.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Uchar.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Uchar.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Uchar.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/prelude.wasm
  Installing _install/bin/main.bc.wasm.assets/prelude.wasm
  Deleting _install/bin/main.bc.wasm.assets/runtime.wasm
  Installing _install/bin/main.bc.wasm.assets/runtime.wasm
  Deleting _install/bin/main.bc.wasm.assets/start.wasm
  Installing _install/bin/main.bc.wasm.assets/start.wasm
  Deleting _install/bin/main.bc.wasm.assets/start-291802e1.wat
  Installing _install/bin/main.bc.wasm.assets/start-291802e1.wat
  Deleting _install/bin/main.bc.wasm.js
  Installing _install/bin/main.bc.wasm.js
  Installing _install/doc/main/README
  $ node _install/bin/main.bc.wasm.js
  v1-1-xxxxx-dirty
  $ echo "(name main)" >> dune-project
  $ echo "(version 0.2.0)" >> dune-project
  $ dune build
  Warning: '--source-map' is enabled but the bytecode program was compiled with no debugging information.
  Warning: Consider passing '-g' option to ocamlc.
  $ node _build/default/src/main.bc.wasm.js
  0.2.0
  $ dune install --prefix _install --display short 2>&1 | sed 's/-[a-f0-9]*[.]wasm/.wasm/'
  Deleting _install/lib/main/META
  Installing _install/lib/main/META
  Deleting _install/lib/main/dune-package
  Installing _install/lib/main/dune-package
  Deleting _install/lib/main/opam
  Installing _install/lib/main/opam
  Deleting _install/bin/main
  Installing _install/bin/main
  Deleting _install/bin/main.bc.wasm.assets/Build_info.wasm
  Installing _install/bin/main.bc.wasm.assets/Build_info.wasm
  Deleting _install/bin/main.bc.wasm.assets/Build_info.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Build_info.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Build_info__Build_info_data.wasm
  Deleting _install/bin/main.bc.wasm.assets/Build_info__Build_info_data.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Build_info__Build_info_data.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalAtomic.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalAtomic.wasm
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalAtomic.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalAtomic.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalFormat.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormat.wasm
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalFormat.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormat.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalFormatBasics.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormatBasics.wasm
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalFormatBasics.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalFormatBasics.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalLazy.wasm
  Installing _install/bin/main.bc.wasm.assets/CamlinternalLazy.wasm
  Deleting _install/bin/main.bc.wasm.assets/CamlinternalLazy.wasm.map
  Installing _install/bin/main.bc.wasm.assets/CamlinternalLazy.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Dune__exe__Main.wasm
  Installing _install/bin/main.bc.wasm.assets/Dune__exe__Main.wasm
  Deleting _install/bin/main.bc.wasm.assets/Dune__exe__Main.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Dune__exe__Main.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Std_exit.wasm
  Installing _install/bin/main.bc.wasm.assets/Std_exit.wasm
  Deleting _install/bin/main.bc.wasm.assets/Std_exit.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Std_exit.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Buffer.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Buffer.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Buffer.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Buffer.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Bytes.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Bytes.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Bytes.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Bytes.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Char.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Char.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Char.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Char.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Int.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Int.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Int.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Int.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Lazy.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Lazy.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Lazy.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Lazy.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__List.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__List.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__List.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__List.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Obj.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Obj.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Obj.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Obj.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Printf.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Printf.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Printf.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Printf.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Seq.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Seq.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Seq.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Seq.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__String.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__String.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__String.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__String.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Sys.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Sys.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Sys.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Sys.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Uchar.wasm
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Uchar.wasm
  Deleting _install/bin/main.bc.wasm.assets/Stdlib__Uchar.wasm.map
  Installing _install/bin/main.bc.wasm.assets/Stdlib__Uchar.wasm.map
  Deleting _install/bin/main.bc.wasm.assets/prelude.wasm
  Installing _install/bin/main.bc.wasm.assets/prelude.wasm
  Deleting _install/bin/main.bc.wasm.assets/runtime.wasm
  Installing _install/bin/main.bc.wasm.assets/runtime.wasm
  Deleting _install/bin/main.bc.wasm.assets/start.wasm
  Installing _install/bin/main.bc.wasm.assets/start.wasm
  Deleting _install/bin/main.bc.wasm.assets/start-291802e1.wat
  Installing _install/bin/main.bc.wasm.assets/start-291802e1.wat
  Deleting _install/bin/main.bc.wasm.js
  Installing _install/bin/main.bc.wasm.js
  Deleting _install/doc/main/README
  Installing _install/doc/main/README
  $ node _build/default/src/main.bc.wasm.js
  0.2.0
