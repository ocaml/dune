let%expect_test "wrapped library with stubs" =
  Archive_creation_behavior.run { wrapped = true; stubs = true };
  [%expect
    {|
    foo.a
    libfoo_stubs.a
    local: exe working
    external: exe working |}]
;;
