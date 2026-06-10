let%expect_test "unwrapped library with stubs" =
  Archive_creation_behavior.run { wrapped = false; stubs = true };
  [%expect
    {|
    libfoo_stubs.a
    local: exe working
    external: exe working |}]
;;
