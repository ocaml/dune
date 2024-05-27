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
    a4ae8e07cf52a9fb38c47c32b6d59fa6
    ---

    INSTALL-COOKIE version 1
    b9cd4cde10e1e3e883032dd57f86c54d
    ---

    TO-PROMOTE version 2
    eba77c5dce16c91bdb3b54c3d48cb5f8
    ---

    COPY-LINE-DIRECTIVE-MAP version 1
    7e311b06ebde9ff1708e4c3a1d3f5633
    ---

    merlin-conf version 5
    49e3a1010b7218f5b229d98991d6d11d
    ---

    INCREMENTAL-DB version 5
    fa67cc9b60c9f3a1b9b1ad93a56df691
    --- |}]
;;
