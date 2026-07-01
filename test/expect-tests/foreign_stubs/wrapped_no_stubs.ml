let%expect_test "wrapped library without stubs" =
  Archive_creation_behavior.run { wrapped = true; stubs = false };
  [%expect
    {|
    foo.a
    local: exe working
    external: exe working |}]
;;
