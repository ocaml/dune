open Import

type 'a t =
  { lib_root : 'a
  ; libexec_root : 'a
  ; bin : 'a
  ; sbin : 'a
  ; share_root : 'a
  ; etc_root : 'a
  ; doc_root : 'a
  ; man : 'a
  }

let to_dyn f { lib_root; libexec_root; bin; sbin; share_root; etc_root; doc_root; man } =
  let open Dyn in
  record
    [ "lib_root", f lib_root
    ; "libexec_root", f libexec_root
    ; "bin", f bin
    ; "sbin", f sbin
    ; "share_root", f share_root
    ; "etc_root", f etc_root
    ; "doc_root", f doc_root
    ; "man", f man
    ]
;;

let make prefix ~relative =
  let lib_root = relative prefix "lib" in
  { lib_root
  ; libexec_root = lib_root
  ; bin = relative prefix "bin"
  ; sbin = relative prefix "sbin"
  ; share_root = relative prefix "share"
  ; man = relative prefix "man"
  ; doc_root = relative prefix "doc"
  ; etc_root = relative prefix "etc"
  }
;;

let make_all a =
  { lib_root = a
  ; libexec_root = a
  ; bin = a
  ; sbin = a
  ; share_root = a
  ; man = a
  ; doc_root = a
  ; etc_root = a
  }
;;

let opam_from_prefix prefix ~relative = make prefix ~relative

let complete x =
  match x.libexec_root with
  | Some _ -> x
  | None -> { x with libexec_root = x.lib_root }
;;

let map ~f x =
  { lib_root = f x.lib_root
  ; libexec_root = f x.libexec_root
  ; bin = f x.bin
  ; sbin = f x.sbin
  ; share_root = f x.share_root
  ; etc_root = f x.etc_root
  ; doc_root = f x.doc_root
  ; man = f x.man
  }
;;

let map2 ~f x y =
  { lib_root = f x.lib_root y.lib_root
  ; libexec_root = f x.libexec_root y.libexec_root
  ; bin = f x.bin y.bin
  ; sbin = f x.sbin y.sbin
  ; share_root = f x.share_root y.share_root
  ; etc_root = f x.etc_root y.etc_root
  ; doc_root = f x.doc_root y.doc_root
  ; man = f x.man y.man
  }
;;

let first_has_priority x y =
  map2 x y ~f:(fun x y ->
    match x with
    | Some _ -> x
    | None -> y)
;;

open Dune_findlib

let ocamlpath = Findlib.Config.ocamlpath_var
let ocamlfind_ignore_dups_in = Findlib.Config.ocamlfind_ignore_dups_in

let to_env_without_path t ~relative =
  [ Ocaml.Env.caml_ld_library_path, relative t.lib_root "stublibs"
  ; ocamlpath, t.lib_root
  ; "OCAMLTOP_INCLUDE_PATH", relative t.lib_root "toplevel"
  ; ocamlfind_ignore_dups_in, t.lib_root
  ; "MANPATH", t.man
  ]
;;

let sep var =
  if var = ocamlpath || var = ocamlfind_ignore_dups_in
  then Some Findlib.Config.ocamlpath_sep
  else None
;;

(* CR-someday Alizter: phantom [unit Roots.t] and dummy [relative] are
   awkward. Cleaner to factor [to_env_without_path] and this list out of a
   single canonical spec list. Not doing it now; leave for a future
   cleanup. *)
let path_vars =
  Env.Var.Set.of_list
    (Env_path.var
     :: List.map ~f:fst (to_env_without_path (make_all ()) ~relative:(fun () _ -> ())))
;;

let add_to_env t env =
  let env =
    to_env_without_path t ~relative:Path.Build.relative
    |> List.fold_left ~init:env ~f:(fun env (var, path) ->
      Env.update env ~var ~f:(fun _PATH ->
        let path_sep = sep var in
        Some (Bin.cons_path ?path_sep (Path.build path) ~_PATH)))
  in
  Env.update env ~var:Env_path.var ~f:(fun _PATH ->
    Some (Bin.cons_path (Path.build t.bin) ~_PATH))
;;

let cons_path env ~var path =
  Env.update env ~var ~f:(fun _PATH ->
    Some (Bin.cons_path ?path_sep:(sep var) (Path.build path) ~_PATH))
;;

(* CR-soon Alizter: path-like env var manipulation is spread across at
   least four ad-hoc implementations, each with different scope, env
   representation, and semantics around the empty value:
   - [Env_path] in [otherlibs/stdune/src/env_path.ml] hardcodes PATH and
     the default separator: [cons], [cons_multi], [path],
     [extend_env_concat_path]. Works on [Env.t] (string values).
   - This module ([cons_path], [extend_env_concat_path_vars]) handles
     every install-managed path var with per-var separators. Also on
     [Env.t].
   - [Pkg_rules.Value_list_env] reimplements the same shape on a
     [Value.t list Env.Map.t], with [extend_concat_path] doing the same
     PATH-only concat dance again.
   - [Pkg_rules.Env_update] then layers opam's [:=]/[+=]/[=:]/[=+]
     operators on top of that representation, with separate rules for
     leading/trailing separators when the variable is initially empty
     (a corner that the other three sidestep, see the CR-someday on the
     [path var: empty value in a] expect test below for an example of
     how surprising this gets).
   The right resolution is probably a single small library that
   parametrises over (env representation, set of recognised path vars,
   per-var separator, empty-value policy) and to which all four call
   sites delegate; the current "pick a world" situation is what made
   [extend_env_concat_path_vars] necessary in the first place. *)
let extend_env_concat_path_vars a b =
  let a =
    Env.Var.Set.fold path_vars ~init:a ~f:(fun var env ->
      match Env.get b var with
      | None -> env
      | Some value ->
        let path_sep = sep var in
        let dirs = Bin.parse_path ?sep:path_sep value in
        Env.update env ~var ~f:(fun init ->
          List.fold_right dirs ~init ~f:(fun dir acc ->
            Some (Bin.cons_path ?path_sep dir ~_PATH:acc))))
  in
  let b = Env.Var.Set.fold path_vars ~init:b ~f:(fun var env -> Env.remove env ~var) in
  Env.extend_env a b
;;

let%test_module "extend_env_concat_path_vars" =
  (module struct
    let env_of_list bindings =
      List.fold_left bindings ~init:Env.empty ~f:(fun env (var, value) ->
        Env.add env ~var ~value)
    ;;

    let print_env env = Env.to_unix env |> List.iter ~f:print_endline
    let go a b = print_env (extend_env_concat_path_vars (env_of_list a) (env_of_list b))

    (* For every variable in [path_vars], demonstrate the same prepend
       semantics. If a new var is added to [path_vars], this test pins
       the expected behaviour. *)
    let%expect_test "every path var: b is prepended to a" =
      Env.Var.Set.iter path_vars ~f:(fun var ->
        let a = Env.add Env.empty ~var ~value:"/from/a" in
        let b = Env.add Env.empty ~var ~value:"/from/b" in
        let result = extend_env_concat_path_vars a b in
        Printf.printf "%s=%s\n" var (Option.value (Env.get result var) ~default:"(none)"));
      [%expect
        {|
        CAML_LD_LIBRARY_PATH=/from/b:/from/a
        MANPATH=/from/b:/from/a
        OCAMLFIND_IGNORE_DUPS_IN=/from/b:/from/a
        OCAMLPATH=/from/b:/from/a
        OCAMLTOP_INCLUDE_PATH=/from/b:/from/a
        PATH=/from/b:/from/a
        |}]
    ;;

    let%expect_test "non-path var: b overrides a" =
      go [ "FOO", "from_a" ] [ "FOO", "from_b" ];
      [%expect {| FOO=from_b |}]
    ;;

    let%expect_test "path var: only a present, a preserved" =
      go [ "PATH", "/a/bin" ] [];
      [%expect {| PATH=/a/bin |}]
    ;;

    let%expect_test "path var: only b present, b passed through" =
      go [] [ "PATH", "/b/bin" ];
      [%expect {| PATH=/b/bin |}]
    ;;

    let%expect_test "path var: multi-dir b keeps its relative order when prepended" =
      go [ "PATH", "/a/bin" ] [ "PATH", "/b1/bin:/b2/bin:/b3/bin" ];
      [%expect {| PATH=/b1/bin:/b2/bin:/b3/bin:/a/bin |}]
    ;;

    let%expect_test "path var: duplicate dirs are not deduplicated" =
      go [ "PATH", "/x" ] [ "PATH", "/x" ];
      [%expect {| PATH=/x:/x |}]
    ;;

    let%expect_test "path var: empty value in b" =
      go [ "PATH", "/a/bin" ] [ "PATH", "" ];
      [%expect {| PATH=/a/bin |}]
    ;;

    (* CR-someday Alizter: the trailing [:] in [/b/bin:] comes from
       [Bin.cons_path "/b/bin" ~_PATH:(Some "")], which treats only [None]
       as "no PATH" and so produces a trailing separator when the existing
       value is the empty string. POSIX reads the empty entry as the
       current directory, so this silently adds [.] to the search path. The
       fix belongs in [Bin.cons_path] (treat [Some ""] like [None]); we pin
       the current behaviour here. *)
    let%expect_test "path var: empty value in a" =
      go [ "PATH", "" ] [ "PATH", "/b/bin" ];
      [%expect {| PATH=/b/bin: |}]
    ;;

    let%expect_test "both inputs empty" =
      go [] [];
      [%expect {| |}]
    ;;
  end)
;;
