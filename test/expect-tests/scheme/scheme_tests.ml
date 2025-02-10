open Stdune
open Memo.O

let print = Printf.printf "%s\n"

module Directory_rules = struct
  type element =
    | File of string
    | Thunk of (unit -> t)

  and t = element Appendable_list.t

  let empty = Appendable_list.empty
  let union = Appendable_list.( @ )
  let concat t = List.fold_left t ~init:empty ~f:union
  let thunk f = Appendable_list.singleton (Thunk f)
  let file f = Appendable_list.singleton (File f)

  let rec force l =
    List.concat_map (Appendable_list.to_list l) ~f:(function
      | File t -> [ t ]
      | Thunk f -> force (f ()))
  ;;
end

module Scheme = struct
  include Scheme

  (* Calls [print] every time any code embedded in the scheme runs, be it a
     [Thunk] constructor or an [Approximation] function.

     The argument of [print] identifies which thunk got run (the path to that
     thunk within the [Scheme.t] value). *)
  let instrument ~print =
    let print path suffix = print (String.concat (List.rev path @ [ suffix ]) ~sep:":") in
    let rec go ~path t : _ Scheme.t =
      match (t : _ Scheme.t) with
      | Empty -> Empty
      | Union (t1, t2) -> Union (go ~path:("l" :: path) t1, go ~path:("r" :: path) t2)
      | Approximation (dirs, rules) ->
        let path = "t" :: path in
        Approximation (dirs, go ~path rules)
      | Finite m -> Finite m
      | Thunk t ->
        Thunk
          (fun () ->
            print path "thunk";
            t ())
    in
    go ~path:[]
  ;;

  (* [collect_rules_simple] is oversimplified in two ways: - it does not share
     the work of scheme flattening, so repeated lookups do repeated work - it
     does not check that approximations are correct

     If approximations are not correct, it will honor the approximation. So
     approximations act like views that prevent the rules from being seen rather
     than from being declared in the first place. *)
  let collect_rules_simple =
    let rec go t ~dir =
      match t with
      | Empty -> Memo.return Directory_rules.empty
      | Union (a, b) ->
        let+ a = go a ~dir
        and+ b = go b ~dir in
        Directory_rules.union a b
      | Approximation (dirs, t) ->
        (match Dune_engine.Dir_set.mem dirs dir with
         | true -> go t ~dir
         | false -> Memo.return Directory_rules.empty)
      | Finite rules ->
        Memo.return
          (match Path.Build.Map.find rules dir with
           | None -> Directory_rules.empty
           | Some rule -> rule)
      | Thunk f ->
        let* t = f () in
        go t ~dir
    in
    go
  ;;

  let evaluate = evaluate ~union:Directory_rules.union

  let get_rules t ~dir =
    let+ rules, _ = Evaluated.get_rules t ~dir in
    Option.value rules ~default:Directory_rules.empty
  ;;
end

module Dir_set = Dune_engine.Dir_set

module Path = struct
  include Path.Build

  let of_string str =
    L.relative
      root
      (match String.split str ~on:'/' with
       | [ "" ] -> []
       | [ "." ] -> []
       | other -> other)
  ;;
end

let record_calls scheme ~f =
  let calls = ref [] in
  let scheme = Scheme.instrument ~print:(fun s -> calls := s :: !calls) scheme in
  let+ res = f scheme in
  Directory_rules.force res, !calls
;;

let print_rules scheme ~dir =
  let* res1, calls1 = record_calls scheme ~f:(Scheme.collect_rules_simple ~dir) in
  let+ res2, calls2 =
    record_calls scheme ~f:(fun scheme ->
      Scheme.evaluate scheme >>= Scheme.get_rules ~dir)
  in
  if not ((res1 : string list) = res2)
  then
    Code_error.raise
      "Naive [collect_rules_simple] gives result inconsistent with [Scheme.evaluate]"
      [ "res1", Dyn.(list string) res1; "res2", Dyn.(list string) res2 ]
  else (
    let print_log log =
      let log =
        match log with
        | [] -> [ "<none>" ]
        | x -> x
      in
      List.iter log ~f:(fun s -> print ("    " ^ s))
    in
    if not ((calls1 : string list) = calls2)
    then (
      print "inconsistent laziness behavior:";
      print "naive calls:";
      print_log calls1;
      print "[evaluate] calls:";
      print_log calls2)
    else (
      print "calls:";
      print_log calls1);
    print "rules:";
    print_log res1)
;;

let run m = Fiber.run (Memo.run m) ~iter:(fun () -> assert false)
let print_rules scheme ~dir = run @@ print_rules scheme ~dir

open Scheme

let%expect_test _ =
  let scheme = Scheme.Thunk (fun () -> Memo.return Scheme.Empty) in
  print_rules scheme ~dir:(Path.of_string "foo/bar");
  [%expect
    {|
calls:
    thunk
rules:
    <none>
|}]
;;

let scheme_all_but_foo_bar =
  Scheme.Approximation
    ( Dir_set.negate (Dir_set.subtree (Path.of_string "foo/bar"))
    , Thunk (fun () -> Memo.return Empty) )
;;

let%expect_test _ =
  print_rules scheme_all_but_foo_bar ~dir:(Path.of_string "unrelated/dir");
  [%expect
    {|
calls:
    t:thunk
rules:
    <none>
|}]
;;

let%expect_test _ =
  print_rules scheme_all_but_foo_bar ~dir:(Path.of_string "foo/bar");
  [%expect
    {|
inconsistent laziness behavior:
naive calls:
    <none>
[evaluate] calls:
    t:thunk
rules:
    <none>
|}]
;;

let%expect_test _ =
  print_rules scheme_all_but_foo_bar ~dir:(Path.of_string "foo/bar/baz");
  [%expect
    {|
inconsistent laziness behavior:
naive calls:
    <none>
[evaluate] calls:
    t:thunk
rules:
    <none>
|}]
;;
