open Stdune

module Git_config_parser = struct
  include Dune_pkg.Private.Git_config_parser

  let binding_to_dyn = Dyn.(pair string string)

  let section_to_dyn { name; arg; bindings } =
    Dyn.record
      [ "name", Dyn.string name
      ; "arg", Dyn.option Dyn.string arg
      ; "bindings", Dyn.list binding_to_dyn bindings
      ]
  ;;

  let to_dyn = Dyn.list section_to_dyn
end

let print_or_fail s =
  match Git_config_parser.parse s with
  | Ok v -> print_endline @@ Dyn.to_string @@ Git_config_parser.to_dyn v
  | Error e -> print_endline e
;;

let%expect_test "parsing simple section" =
  let config = {|[foo]
    bar = baz
    |} in
  print_or_fail config;
  [%expect {|
  [ { name = "foo"; arg = None; bindings = [ ("bar", "baz") ] } ]
  |}]
;;

let%expect_test "parsing without spaces" =
  let config = {|[foo]
bar=baz|} in
  print_or_fail config;
  [%expect {|
  [ { name = "foo"; arg = None; bindings = [ ("bar", "baz") ] } ]
  |}]
;;

let%expect_test "parsing with space" =
  let config = {|[foo]
    bar = baz qux
    |} in
  print_or_fail config;
  [%expect {|
  [ { name = "foo"; arg = None; bindings = [ ("bar", "baz qux") ] } ]
  |}]
;;

let%expect_test "parsing with equal" =
  let config = {|[foo]
    bar = baz=qux
    |} in
  print_or_fail config;
  [%expect {|
  [ { name = "foo"; arg = None; bindings = [ ("bar", "baz=qux") ] } ]
  |}]
;;

let%expect_test "parsing without equal" =
  let config = {|[foo]
    enabled
    |} in
  print_or_fail config;
  [%expect {|
  [ { name = "foo"; arg = None; bindings = [ ("enabled", "true") ] } ]
  |}]
;;

let%expect_test "parsing multiple bindings" =
  let config = {|[foo]
    bar = baz
    foo = baz
  |} in
  print_or_fail config;
  [%expect
    {|
  [ { name = "foo"; arg = None; bindings = [ ("bar", "baz"); ("foo", "baz") ] }
  ]
  |}]
;;

let%expect_test "parsing duplicate bindings" =
  let config = {|[foo]
    bar = baz
    bar = qux
  |} in
  print_or_fail config;
  [%expect
    {|
  [ { name = "foo"; arg = None; bindings = [ ("bar", "baz"); ("bar", "qux") ] }
  ]
  |}]
;;

let%expect_test "comments" =
  let config = {|[foo]
    #set = wrong
    ;set = incorrect
    set = correct
  |} in
  print_or_fail config;
  [%expect {|
  [ { name = "foo"; arg = None; bindings = [ ("set", "correct") ] } ]
  |}]
;;

let%expect_test "parsing no bindings" =
  let config = {|[empty]
  [filled]
  baz=qux
  |} in
  print_or_fail config;
  [%expect
    {|
  [ { name = "empty"; arg = None; bindings = [] }
  ; { name = "filled"; arg = None; bindings = [ ("baz", "qux") ] }
  ]
  |}]
;;

let%expect_test "parsing multiple sections" =
  let config = {|[foo]
    bar = baz
    
   [bar]
   foo = baz
  |} in
  print_or_fail config;
  [%expect
    {|
  [ { name = "foo"; arg = None; bindings = [ ("bar", "baz") ] }
  ; { name = "bar"; arg = None; bindings = [ ("foo", "baz") ] }
  ]
  |}]
;;

let%expect_test "parsing whitespaces" =
  let config = {|

  [foo]


    bar = baz

  [qux]

    quux = corge
  |} in
  print_or_fail config;
  [%expect
    {|
  [ { name = "foo"; arg = None; bindings = [ ("bar", "baz") ] }
  ; { name = "qux"; arg = None; bindings = [ ("quux", "corge") ] }
  ]
  |}]
;;

let%expect_test "parsing named section" =
  let config = {|[foo "name"]
    bar = baz
    |} in
  print_or_fail config;
  [%expect
    {|
  [ { name = "foo"; arg = Some "name"; bindings = [ ("bar", "baz") ] } ]
  |}]
;;

let%expect_test "case sensitivity" =
  let config = {|[fOO "nAmE"]
    bar = baz
    |} in
  print_or_fail config;
  [%expect
    {|
  [ { name = "foo"; arg = Some "nAmE"; bindings = [ ("bar", "baz") ] } ]
  |}]
;;
