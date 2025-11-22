open Dune_tests_common
open Stdune
module Ocamlobjinfo = Dune_rules.For_tests.Ocamlobjinfo
module Module_name = Dune_lang.Module_name

let () = init ()

let fixture =
  {ocamlobjinfo|
File _boot/stdune__Env.cmi
Unit name: Stdune__Env
Interfaces imported:
        bd468c1984f53f5b1d3dadf52a621031        Stdune__Env
        ba308f46e67d900971a536229f7f4173        Stdune__String
        421e81602ba8b4117354ae965fbcdb9b        Stdune__Set_intf
        e71746f44d47934ce121640bcb2f82d1        Stdune__Seq
        d5f56ce66eefdb02b6328534c965b2a4        Stdune__Result
        06eac0380458db0b6b9dbf534491bb7d        Stdune__Map_intf
        143081f49dab6fc606ae7041a2bb6ab1        Stdune__Hashtbl_intf
        30c8429b4c4dacb8b40f9032dfc90a7c        Stdune__Hashtbl
        2f561d28974f1c76974b217015d894ff        Stdune__Hashable
        f01519520ce7398ca472c40bcdd37916        Stdune__Either
        26c7c23a3f08949c9234b39a8456e0c4        Stdune__Dune_either
        6c4aad0453a73862d11f3e5a6484eb83        Stdune__Comparator
        78d20a1f0a585a77c0b794e2576c5ce1        Stdune__Comparable_intf
        95b5a54d9e83e8c31cf45102cda75e48        Stdune__
        e99839b765b60b06e594103d43eb2fc3        Stdlib__Uchar
        3bc9847742f040684ab499418b7edf05        Stdlib__StringLabels
        22ab37e5d785c94907a48029f32d8e8b        Stdlib__Seq
        41f1535996cca328bb845b07f8302854        Stdlib__Format
        46032d55ce31c121a36c0c831eb303d0        Stdlib__Either
        22513dfb5b42771d934b5ab03c3e52e2        Stdlib__Domain
        fb4e94053b07bc3831961cf2dd4bfa5f        Stdlib__Buffer
        b7726274865ffd07290ef11351fb84fe        Stdlib
        3d06cb761b7b5c8b1688884de82f6f44        Pp
        95df35f1ae12a6752513bc2d6dd1e2aa        Ordering
        40da7899f44eca869bcd9216c9f03eb3        Dyn
        863a7f5288599b181f046f0f55277b0f        CamlinternalFormatBasics
File _build/default/otherlibs/stdune/src/.stdune.objs/byte/stdune__Env.cmo
Unit name: Stdune__Env
Interfaces imported:
        03e897aee435213573adf13c80fcb505        Unix
        a37d011736ba15fcb93e301d6e9668d2        Stdune__String
        b51e9564a437f5dab85ecd7dbeb5fe9e        Stdune__Set_intf
        5fa01009dd8a8dc4b5aa020829607a29        Stdune__Seq
        c4077e4346d1333fb5a5d86cb1b29d00        Stdune__Result
        533fd86efff7f4531b0b06618b7035fc        Stdune__Poly
        fb775f8b752e8168d73ada4c504f02b6        Stdune__Map_intf
        fae59d6691dc238fd60bd45af4965361        Stdune__Map
        73b7f60f54e3ce7915dc84d6e67ed356        Stdune__Loc0
        e17e834cc6ebc2c4dd1c00550d905672        Stdune__List
        dacde49bf9f97ac658c92aed71b8c785        Stdune__Lexbuf
        c1d2054a047001a99870331b34bcabfb        Stdune__Hashtbl_intf
        2355b12e6f04e2effaeed7fcf7788457        Stdune__Hashtbl
        f78523139161a07f69c9bb172c66d2d6        Stdune__Hashable
        e658ad11f9a5db6440d11c79d9c50742        Stdune__Env
        9fd88bd5b1dfb61f9d7713cda581d288        Stdune__Either
        c6c90e99c836610dda40af3b03120714        Stdune__Dune_either
        99b7fcf93dc56dcc938e4886b46ce3b3        Stdune__Comparator
        4ab9970cf012f4719f898657f92b5780        Stdune__Comparable_intf
        ed5bf8d301e6efbb9bb75d08e9fda6ed        Stdune__Comparable
        b280176bcf92e00bb31e82e836b79478        Stdune__Code_error
        2fdcab3c50cae1145cd9ad5a42fef2b0        Stdune__Array
        56b745aad68436e15c04a81a49a64bc2        Stdune__
        e99839b765b60b06e594103d43eb2fc3        Stdlib__Uchar
        37ffc17421e2ae344263e703f9b97a7f        Stdlib__Sys
        3bc9847742f040684ab499418b7edf05        Stdlib__StringLabels
        22ab37e5d785c94907a48029f32d8e8b        Stdlib__Seq
        48aae94bed7b9364737dec491dd5504f        Stdlib__Printf
        2b62d36b226e41c97dbde849f2884dd5        Stdlib__ListLabels
        79bfb1f841143482726839838f8b6560        Stdlib__Lexing
        41f1535996cca328bb845b07f8302854        Stdlib__Format
        46032d55ce31c121a36c0c831eb303d0        Stdlib__Either
        22513dfb5b42771d934b5ab03c3e52e2        Stdlib__Domain
        d3484881d7abcc6fad213e2616c0406e        Stdlib__Complex
        fb4e94053b07bc3831961cf2dd4bfa5f        Stdlib__Buffer
        5bd8a0c0af94a79b082f9c2fdd62d630        Stdlib__Bigarray
        c754c627bac3f2948b8da959a4caa4e5        Stdlib__ArrayLabels
        b7726274865ffd07290ef11351fb84fe        Stdlib
        b0a9d2486ded60332f67def162ad115a        Pp
        74e26938103410d4bef478dee9a67659        Ordering
        b6bda25e76f7a0790682d620ddd2e240        Dyn
        863a7f5288599b181f046f0f55277b0f        CamlinternalFormatBasics
Required globals:
        Dyn
        Stdlib__Printf
        Stdlib__Sys
        Stdune__Array
        Stdune__Code_error
        Stdune__Comparable
        Stdune__List
        Stdune__Poly
        Stdune__String
        Unix
Uses unsafe features: no
Force link: no
|ocamlobjinfo}
;;

let parse s = Ocamlobjinfo.parse s |> Dyn.list Ocamlobjinfo.to_dyn |> print_dyn

let%expect_test _ =
  parse fixture;
  [%expect
    {|
    [ { impl = set {}
      ; intf =
          set
            { "camlinternalFormatBasics"
            ; "dyn"
            ; "ordering"
            ; "pp"
            ; "stdlib"
            ; "stdlib__Buffer"
            ; "stdlib__Domain"
            ; "stdlib__Either"
            ; "stdlib__Format"
            ; "stdlib__Seq"
            ; "stdlib__StringLabels"
            ; "stdlib__Uchar"
            ; "stdune__"
            ; "stdune__Comparable_intf"
            ; "stdune__Comparator"
            ; "stdune__Dune_either"
            ; "stdune__Either"
            ; "stdune__Env"
            ; "stdune__Hashable"
            ; "stdune__Hashtbl"
            ; "stdune__Hashtbl_intf"
            ; "stdune__Map_intf"
            ; "stdune__Result"
            ; "stdune__Seq"
            ; "stdune__Set_intf"
            ; "stdune__String"
            }
      }
    ; { impl = set {}
      ; intf =
          set
            { "camlinternalFormatBasics"
            ; "dyn"
            ; "ordering"
            ; "pp"
            ; "stdlib"
            ; "stdlib__ArrayLabels"
            ; "stdlib__Bigarray"
            ; "stdlib__Buffer"
            ; "stdlib__Complex"
            ; "stdlib__Domain"
            ; "stdlib__Either"
            ; "stdlib__Format"
            ; "stdlib__Lexing"
            ; "stdlib__ListLabels"
            ; "stdlib__Printf"
            ; "stdlib__Seq"
            ; "stdlib__StringLabels"
            ; "stdlib__Sys"
            ; "stdlib__Uchar"
            ; "stdune__"
            ; "stdune__Array"
            ; "stdune__Code_error"
            ; "stdune__Comparable"
            ; "stdune__Comparable_intf"
            ; "stdune__Comparator"
            ; "stdune__Dune_either"
            ; "stdune__Either"
            ; "stdune__Env"
            ; "stdune__Hashable"
            ; "stdune__Hashtbl"
            ; "stdune__Hashtbl_intf"
            ; "stdune__Lexbuf"
            ; "stdune__List"
            ; "stdune__Loc0"
            ; "stdune__Map"
            ; "stdune__Map_intf"
            ; "stdune__Poly"
            ; "stdune__Result"
            ; "stdune__Seq"
            ; "stdune__Set_intf"
            ; "stdune__String"
            ; "unix"
            }
      }
    ]
    |}]
;;

let archive_fixture =
  {ocamlobjinfo|
File /path/to/mylib.cma
Force custom: no
Extra C object files:
Extra C options:
Extra dynamically-loaded libraries:
Unit name: Foo
Interfaces imported:
	--------------------------------	Bar
	--------------------------------	Stdlib
Implementations imported:
Unit name: Bar
Interfaces imported:
	--------------------------------	Baz
	--------------------------------	Stdlib
Implementations imported:
Unit name: Helper
Interfaces imported:
	--------------------------------	Stdlib
Implementations imported:
|ocamlobjinfo}
;;

let parse_archive s =
  let modules = Ocamlobjinfo.parse_archive s in
  Module_name.Unique.Set.to_list modules
  |> List.map ~f:Module_name.Unique.to_string
  |> String.Set.of_list
  |> String.Set.to_dyn
  |> print_dyn
;;

let%expect_test "parse_archive extracts unit names" =
  parse_archive archive_fixture;
  [%expect
    {|
    set { "bar"; "foo"; "helper" }
    |}]
;;

let%expect_test "regression test" =
  parse
    {fixture|
File foo/bar.cmo
Unit name: Bar
Interfaces imported:
	03e897aee435213573adf13c80fcb505	M1
	863a7f5288599b181f046f0f55277b0f	M2
Required globals:
	X
Uses unsafe features: no
Force link: no
File foo/baz.cmo
Unit name: Baz
Interfaces imported:
	1b835c1e0367d04fd16979463a84a7cd	M3
	6b06eead182e9e338552e756d414f9e8	M4
Required globals:
	Y
Uses unsafe features: no
Force link: no
|fixture};
  [%expect {| [ { impl = set {}; intf = set { "m1"; "m2" } } ] |}]
;;
