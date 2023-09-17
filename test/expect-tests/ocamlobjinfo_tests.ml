open! Stdune
open Dune_tests_common
module Ocamlobjinfo = Dune_rules.For_tests.Ocamlobjinfo

let () = init ()

let fixture =
  {ocamlobjinfo|
File _build/install/default/lib/dune/_stdune/stdune__Env.cmx
Name: Stdune__Env
CRC of implementation: b678d7aae434ca3158721e3a37a15776
Globals defined:
        Stdune__Env
Interfaces imported:
        053326e853ce10e1fadf8d891f08f891        Unix
        596c497318b5c3057b47b9d6747ef5d1        Uchar
        3fe6d98e0634486be22d9de07aa0709a        Sys
        6339e2b71e8c583a81e808954faf6818        StringLabels
        953d4ea121ff79e9719730997e04436d        Stdune__String
        744ae4e7c80910dd9302bf207d11274f        Stdune__Sexp_intf
        3a8d88a2c7628492ca76328610a53b04        Stdune__Sexp
        788120b20799b5148dc44c2effd9db08        Stdune__Set_intf
        764302df6c51161e13ccc4553710003d        Stdune__Set
        7577d25061f730d87e8e0ab574216d9c        Stdune__Result
        4d34756156087d6e6530be0d3275bde4        Stdune__Path
        e8eebf0307152a528b7d97ebdf37d1dc        Stdune__Ordering
        1fb9ae8c1fb73c55a857f02869cfbeab        Stdune__Map_intf
        96daa1ecff8c9bc6c5aadf39201c2f0d        Stdune__Map
        5b332ee2108ade34f2abc1448318c5a0        Stdune__Loc
        721c54b94b8c3b89fffc8587041c241a        Stdune__List
        0c6e26bc5bc285a87ab7304ee5f3bf0a        Stdune__Hashtbl_intf
        8df5a38cb2f28d4cf23f6cd8a6c4c923        Stdune__Hashtbl
        0ff241a400cabb742a8d681c7132c350        Stdune__Hashable
        b438dbe8d4c60ca1bb06d04c7bc95652        Stdune__Exn
        a1b4f657ccceb16196f104a53a0e199a        Stdune__Env
        7643682d2d95acbcf8834a351a1e2779        Stdune__Either
        826226f09894dc48694a431d13b9e541        Stdune__Comparator
        69861cabc5a73becd98269ce298eaa59        Stdune__Bin
        cbf0ce887ccceaff96f4a22a8f057799        Stdune__Array
        37cf8dd4fc4be5636ff9f78604107f8b        Stdune__
        28a12def19edf36c317c30fafcc03d6d        Set
        e5dfd0ca6436c8abad976fc9e914999a        Printf
        1b461321ebcc8e419f24eb531c5ac7ac        Printexc
        9b04ecdc97e5102c1d342892ef7ad9a2        Pervasives
        db5fc31b815ab3040d5a9940a91712c4        MoreLabels
        8b8de381501aa7862270c15619322ee7        Map
        f4e829075d9d0bb7de979cfc49c2600b        ListLabels
        0971650cdf1fa8e506e733e9a5da2628        Lexing
        0a88e320f172d3413ba0d5e0f9c70ccd        Hashtbl
        1a17539924469551f027475153d4d3b5        Format
        a4afff2bf4082efda68a6a65cf31f8e2        Stdlib__Result_compat
        de733b926f4af640c957c8129aba4139        Stdlib__Result
        ba19641102c1711bdb2476bb8b8dbe32        Stdlib__
        7b10d1bd2d88af9c1da841149c988d94        Stdlib
        cd4856c93f21942683ce190142e88396        Complex
        79ae8c0eb753af6b441fe05456c7970b        CamlinternalFormatBasics
        4ff98b0650eef9c38ee9c9930e0c3e9b        CamlinternalBigarray
        9c9b3639d23d7746c571cdf04646eb29        Buffer
        c4974e11dd7c941c002b826edc727de8        ArrayLabels
Implementations imported:
        e28bcdad48b0cb1739e47106149016cb        Unix
        3c11d6a8ae012d6541b58cecff4809d5        Sys
        --------------------------------        Stdune__String
        --------------------------------        Stdune__Sexp
        --------------------------------        Stdune__Set
        --------------------------------        Stdune__Map
        --------------------------------        Stdune__List
        --------------------------------        Stdune__Exn
        --------------------------------        Stdune__Bin
        --------------------------------        Stdune__Array
        a3cdcb16ec3b460d51a685302e993c0b        Printf
Clambda approximation:
  _
Currying functions: 3 2
Apply functions: 3 2
Send functions:
Force link: no
|ocamlobjinfo}
;;

let parse s = Ocamlobjinfo.parse s |> Ocamlobjinfo.to_dyn |> print_dyn

let%expect_test _ =
  parse fixture;
  [%expect
    {|
{ impl =
    set
      { "printf"
      ; "stdune__Array"
      ; "stdune__Bin"
      ; "stdune__Exn"
      ; "stdune__List"
      ; "stdune__Map"
      ; "stdune__Set"
      ; "stdune__Sexp"
      ; "stdune__String"
      ; "sys"
      ; "unix"
      }
; intf =
    set
      { "arrayLabels"
      ; "buffer"
      ; "camlinternalBigarray"
      ; "camlinternalFormatBasics"
      ; "complex"
      ; "format"
      ; "hashtbl"
      ; "lexing"
      ; "listLabels"
      ; "map"
      ; "moreLabels"
      ; "pervasives"
      ; "printexc"
      ; "printf"
      ; "set"
      ; "stdlib"
      ; "stdlib__"
      ; "stdlib__Result"
      ; "stdlib__Result_compat"
      ; "stdune__"
      ; "stdune__Array"
      ; "stdune__Bin"
      ; "stdune__Comparator"
      ; "stdune__Either"
      ; "stdune__Env"
      ; "stdune__Exn"
      ; "stdune__Hashable"
      ; "stdune__Hashtbl"
      ; "stdune__Hashtbl_intf"
      ; "stdune__List"
      ; "stdune__Loc"
      ; "stdune__Map"
      ; "stdune__Map_intf"
      ; "stdune__Ordering"
      ; "stdune__Path"
      ; "stdune__Result"
      ; "stdune__Set"
      ; "stdune__Set_intf"
      ; "stdune__Sexp"
      ; "stdune__Sexp_intf"
      ; "stdune__String"
      ; "stringLabels"
      ; "sys"
      ; "uchar"
      ; "unix"
      }
}
|}]
;;
