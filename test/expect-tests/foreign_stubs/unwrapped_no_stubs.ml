let%expect_test "unwrapped library without stubs" =
  Archive_creation_behavior.run { wrapped = false; stubs = false };
  [%expect
    {|
    local: exe working
    external: exe working |}]
;;
