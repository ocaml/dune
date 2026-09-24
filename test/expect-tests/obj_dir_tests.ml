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

let%expect_test "CM directories preserve representation and visibility" =
  let local =
    Obj_dir.make_lib
      ~dir:(Path.Build.relative Path.Build.root "default/lib")
      ~has_private_modules:true
      ~private_lib:true
      (Dune_lang.Lib_name.Local.of_string "foo")
  in
  let local_as_path = Obj_dir.of_local local in
  let external_root = Path.of_string "prefix/lib/foo" in
  let external_ =
    Obj_dir.convert_to_external local ~dir:external_root ~has_private_modules:true
  in
  let cases : (Lib_mode.Cm_kind.t * string * string * string) list =
    [ Ocaml Cmi, "byte", ".public_cmi", ".private"
    ; Ocaml Cmo, "byte", "", ""
    ; Ocaml Cmx, "native", "", ""
    ; Melange Cmi, "melange", "melange/.public_cmi_melange", "melange/.private"
    ; Melange Cmj, "melange", "melange", "melange"
    ]
  in
  let correct =
    List.for_all cases ~f:(fun (kind, local_dir, public_dir, private_dir) ->
      let expected_local = Path.Build.relative (Obj_dir.obj_dir local) local_dir in
      List.for_all
        [ Dune_lang.Visibility.Public, public_dir; Private, private_dir ]
        ~f:(fun (visibility, external_dir) ->
          let expected_external =
            match external_dir with
            | "" -> external_root
            | dir -> Path.relative external_root dir
          in
          Path.Build.equal (Obj_dir.cm_dir local kind visibility) expected_local
          && Path.equal
               (Obj_dir.cm_dir local_as_path kind visibility)
               (Path.build expected_local)
          && Path.equal (Obj_dir.cm_dir external_ kind visibility) expected_external))
  in
  printfn "all modes, visibilities and representations agree: %b" correct;
  let without_private =
    Obj_dir.convert_to_external local ~dir:external_root ~has_private_modules:false
  in
  let private_cmi_errors =
    List.for_all [ Lib_mode.Cm_kind.Ocaml Cmi; Melange Cmi ] ~f:(fun kind ->
      try
        ignore (Obj_dir.cm_dir without_private kind Private : Path.t);
        false
      with
      | Code_error.E { message; _ } -> String.equal message "External.cm_dir")
  in
  printfn "missing private CMI directories still raise: %b" private_cmi_errors;
  [%expect
    {|
    all modes, visibilities and representations agree: true
    missing private CMI directories still raise: true
    |}]
;;
