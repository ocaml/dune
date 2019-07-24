(* -*- tuareg -*- *)

open Stdune

let () =
  Path.Build.set_build_dir (Path.Build.Kind.of_string (Filename.get_temp_dir_name ()));;

let dir =
  let rand_digits () =
    let rand = Random.State.(bits (make_self_init ()) land 0xFFFFFF) in
    Printf.sprintf "%06x" rand
  in
  let mk_temp_dir pat =
    let root = Filename.get_temp_dir_name ()
    in
    let raise_err msg = raise (Sys_error msg) in
    let rec loop count =
      if count < 0 then raise_err "mk_temp_dir: too many failing attemps"
      else
        let dir = Printf.sprintf "%s%s" pat (rand_digits ()) in
        try Unix.mkdir (root ^ "/" ^ dir) 0o700 ; Path.of_string dir with
        | Unix.Unix_error (Unix.EEXIST, _, _) ->
            loop (count - 1)
        | Unix.Unix_error (Unix.EINTR, _, _) ->
            loop count
        | Unix.Unix_error (e, _, _) ->
            raise_err ("mk_temp_dir: " ^ Unix.error_message e)
    in
    loop 1000
  in
  (mk_temp_dir ".dune-memory_unit-test_");;

let () =
  Path.mkdir_p dir;;

let make_file p = let path = (Path.of_string ((Path.to_string dir) ^ "/" ^ p)) in
  Io.write_file path p; path;;
let memory = Result.ok_exn (Dune_memory.make ~root:(Path.of_string ((Path.to_string dir) ^ "/root/v2")) ())

let clean_path p =
  let prefix = Path.to_string dir in
  Path.of_string (Option.value_exn (String.drop_prefix ~prefix (Path.to_string p)))

let clean_promotion = function
  | Dune_memory.Already_promoted (p1, p2) -> Dune_memory.Already_promoted (clean_path p1, clean_path p2)
  | Dune_memory.Promoted (p1, p2) -> Dune_memory.Promoted (clean_path p1, clean_path p2)
  | Dune_memory.Hash_mismatch (p, d1, d2) -> Dune_memory.Hash_mismatch (clean_path p, d1, d2)

(* Promote a file twice and check we can search it *)
let file1 = make_file "file1";;
let metadata = [Sexp.List [Sexp.Atom "test"; Sexp.Atom "metadata"]];;
let key = Dune_memory.key [] metadata [file1];;
let%expect_test _ =
  let f p = print_endline (Dune_memory.promotion_to_string (clean_promotion p)) in
  List.iter ~f (Dune_memory.promote memory [(file1, Digest.file file1)] key metadata None);
  List.iter ~f (Dune_memory.promote memory [(file1, Digest.file file1)] key metadata None);
  (match Result.ok_exn (Dune_memory.search memory key) with
  |  (stored_metadata, [(original, promoted)]) ->
      if not (List.for_all2 ~f:Sexp.equal stored_metadata metadata) then
        failwith "Metadata mismatch"
      else if Path.equal original file1 then
        if Io.compare_files promoted file1 = Ordering.Eq then ()
        else failwith "promoted file content does not match"
      else failwith "original file path does not match"
  | _ -> failwith "wrong number of file found");
  (* Check write permissions where removed *)
  assert ((Unix.stat (Path.to_string file1)).st_perm land 0o222 = 0);
  Path.rm_rf dir;
  [%expect{|
    /file1 promoted as /root/v2/files/82/826e8142e6baabe8af779f5f490cf5f5.1
    /file1 already promoted as /root/v2/files/82/826e8142e6baabe8af779f5f490cf5f5.1 |}]
