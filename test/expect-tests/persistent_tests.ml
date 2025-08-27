open Stdune
module Persistent = Dune_util.Persistent
module Digest = Dune_digest

let test (type a) (module Persistent : Persistent.Desc with type t = a) (example : a) =
  let digest = Digest.generic example |> Digest.to_string in
  printfn "%s version %d\n%s\n---\n" Persistent.name Persistent.version digest
;;

let%expect_test "persistent digests" =
  Persistent.test_examples ()
  (* These digests are to make sure that we're bumping the version whenever we
     change the format of the values stored with [Persistent].

     The usual workflow goes something like this:

     1. The format of [Persistent.t] changes
     2. The new value is reflected by the value returned [test_example]
     3. The digest in this test suite changes and the test therefore fails

     To fix the test, the correct thing to do is to bump the appropriate
     version number where the persistent module is defined *)
  |> Stdlib.Seq.iter (fun (Persistent.T (desc, example)) -> test desc example);
  [%expect
    {|
    PROMOTED-TO-DELETE version 3
    65e543aaf5ccc8148d50a1305aa3622b
    ---

    DIGEST-DB version 7
    48031a13035ffa6b93b6b79ce277d39c
    ---

    INSTALL-COOKIE version 3
    da4ce847dd41df462849adecfe43f4eb
    ---

    TO-PROMOTE version 3
    f2d6070d92c27497a6c2d89782d81c99
    ---

    COPY-LINE-DIRECTIVE-MAP version 2
    72eb282c39bb084e69d6bd1615b8aaec
    ---

    CRAM-RESULT version 1
    65e543aaf5ccc8148d50a1305aa3622b
    ---

    merlin-conf version 7
    a14a4700929a15bb2030e36f71e66d20
    ---

    INCREMENTAL-DB version 6
    5d401c8cac2683cee736494457885f1f
    ---
    |}]
;;
