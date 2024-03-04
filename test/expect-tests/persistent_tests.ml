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
    PROMOTED-TO-DELETE version 2
    c5e35411975aef719f04a574b4ff5940
    ---

    DIGEST-DB version 6
    a6df9e528c50debc9264b7a95489392e
    ---

    INSTALL-COOKIE version 1
    b9cd4cde10e1e3e883032dd57f86c54d
    ---

    TO-PROMOTE version 2
    eba77c5dce16c91bdb3b54c3d48cb5f8
    ---

    COPY-LINE-DIRECTIVE-MAP version 1
    7dac5b11f6f654bb6f230392493b363f
    ---

    merlin-conf version 4
    782b1c9ea57a40a427f80fa24ba6d853
    ---

    INCREMENTAL-DB version 5
    1cc656a4502ef88e70adab1f3c9a868e
    --- |}]
;;
