open Stdune
module Obj_dir = Dune_rules.Obj_dir
module Lib_mode = Dune_lang.Lib_mode

let () = Dune_tests_common.init ()

let paths obj_dir mode =
  Obj_dir.all_obj_dirs obj_dir ~mode
  |> List.map ~f:(fun path -> Dyn.string (Path.to_string path))
  |> Dyn.list Fun.id
;;

let%expect_test "external object directories with dedicated public CMI directories" =
  let obj_dir =
    Obj_dir.make_lib
      ~dir:(Path.Build.relative Path.Build.root "default/lib")
      ~has_private_modules:false
      ~private_lib:true
      (Dune_lang.Lib_name.Local.of_string "foo")
    |> Obj_dir.convert_to_external
         ~dir:(Path.of_string "prefix/lib/foo")
         ~has_private_modules:false
  in
  Dune_tests_common.print_dyn
    (Dyn.record
       [ "ocaml byte", paths obj_dir (Lib_mode.Ocaml Ocaml.Mode.Byte)
       ; "ocaml native", paths obj_dir (Lib_mode.Ocaml Ocaml.Mode.Native)
       ; "melange", paths obj_dir Lib_mode.Melange
       ]);
  [%expect
    {|
    { ocaml byte = [ "prefix/lib/foo"; "prefix/lib/foo/.public_cmi" ]
    ; ocaml native = [ "prefix/lib/foo"; "prefix/lib/foo/.public_cmi" ]
    ; melange =
        [ "prefix/lib/foo/melange"
        ; "prefix/lib/foo/melange/.public_cmi_melange"
        ]
    }
    |}]
;;
