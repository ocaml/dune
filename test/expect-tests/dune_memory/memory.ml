(* -*- tuareg -*- *)

open Stdune

let () = Dune_tests_common.init ()

let dir =
  let rand_digits () =
    let rand = Random.State.(bits (make_self_init ()) land 0xFFFFFF) in
    Printf.sprintf "%06x" rand
  in
  let mk_temp_dir pat =
    let root = Filename.get_temp_dir_name () in
    let raise_err msg = raise (Sys_error msg) in
    let rec loop count =
      if count < 0 then
        raise_err "mk_temp_dir: too many failing attemps"
      else
        let dir = Printf.sprintf "%s%s" pat (rand_digits ()) in
        try
          Unix.mkdir (root ^ "/" ^ dir) 0o700;
          Path.of_string dir
        with
        | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
        | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
        | Unix.Unix_error (e, _, _) ->
          raise_err ("mk_temp_dir: " ^ Unix.error_message e)
    in
    loop 1000
  in
  mk_temp_dir ".dune-memory_unit-test_"

let () = Path.mkdir_p dir

let make_file p =
  let path = Path.of_string (Path.to_string dir ^ "/" ^ p) in
  Io.write_file path p;
  path

let memory =
  match
    Dune_memory.make
      ~root:(Path.of_string (Path.to_string dir ^ "/root/v2"))
      ()
  with
  | Result.Ok memory ->
    Dune_memory.Memory.set_build_dir memory
      (Path.of_string (Path.External.to_string (Path.External.cwd ())))
  | Result.Error msg -> User_error.raise [ Pp.textf "%s" msg ]

let clean_path p =
  let prefix = Path.to_string dir in
  Path.of_string
    (Option.value_exn (String.drop_prefix ~prefix (Path.to_string p)))

let promotion_to_string p =
  let p =
    match p with
    | Dune_memory.Already_promoted (p1, p2) ->
      Dune_memory.Already_promoted (p1, clean_path p2)
    | Dune_memory.Promoted (p1, p2) -> Dune_memory.Promoted (p1, clean_path p2)
  in
  Option.value_exn
    (String.drop_prefix ~prefix:(Path.to_string dir)
       (Dune_memory.promotion_to_string p))

(* Promote a file twice and check we can search it *)
let file1 = make_file "file1"

let metadata = [ Sexp.List [ Sexp.Atom "test"; Sexp.Atom "metadata" ] ]

(* The key is internal to dune and opaque to us, we can use anything *)
let key = Digest.generic "dummy-hash"

let%expect_test _ =
  let f p = print_endline (promotion_to_string p)
  and stats = Unix.stat (Path.to_string file1) in
  let open Result.O in
  match
    Dune_memory.Memory.promote memory
      [ (file1, Digest.file_with_stats file1 stats) ]
      key metadata None
    >>= fun promotions ->
    List.iter ~f promotions;
    Dune_memory.Memory.promote memory
      [ (file1, Digest.file_with_stats file1 stats) ]
      key metadata None
    >>= fun promotions ->
    List.iter ~f promotions;
    Dune_memory.Memory.search memory key
    >>| fun searched ->
    ( match searched with
    | stored_metadata, [ file ] ->
      let in_the_build_directory =
        Path.of_local (Path.Build.local file.in_the_build_directory)
      in
      if not (List.for_all2 ~f:Sexp.equal stored_metadata metadata) then
        failwith "Metadata mismatch"
      else if Path.equal in_the_build_directory file1 then
        if Io.compare_files file.in_the_memory file1 = Ordering.Eq then
          ()
        else
          failwith "promoted file content does not match"
      else
        failwith
          (Format.asprintf "original file path does not match: %a != %a"
             Path.pp in_the_build_directory Path.pp file1)
    | _ -> failwith "wrong number of file found" );
    (* Check write permissions where removed *)
    assert ((Unix.stat (Path.to_string file1)).st_perm land 0o222 = 0);
    Path.rm_rf dir
  with
  | Result.Ok () ->
    [%expect
      {|
/file1 promoted as /root/v2/files/0d/0d6600c511071ec1cc21942910665669.1
/file1 already promoted as /root/v2/files/0d/0d6600c511071ec1cc21942910665669.1
|}]
  | Result.Error s -> failwith s
