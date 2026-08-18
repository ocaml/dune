let create path =
  let fd = Unix.openfile path [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in
  Unix.close fd
;;

let expect_denied f =
  match f () with
  | () -> exit 1
  | exception Unix.Unix_error ((EACCES | EPERM | EXDEV), _, _) -> ()
;;

let access_rights allowed denied =
  Unix.truncate (Filename.concat allowed "truncate") 0;
  Unix.unlink (Filename.concat allowed "delete");
  Unix.symlink "target" (Filename.concat allowed "symlink");
  Unix.rename
    (Filename.concat allowed "rename-source")
    (Filename.concat allowed "rename-target");
  Unix.rmdir (Filename.concat allowed "remove-dir");
  Unix.mkdir (Filename.concat allowed "mkdir") 0o700;
  expect_denied (fun () -> Unix.truncate (Filename.concat denied "truncate") 0);
  expect_denied (fun () -> Unix.unlink (Filename.concat denied "delete"));
  expect_denied (fun () -> Unix.symlink "target" (Filename.concat denied "symlink"));
  expect_denied (fun () ->
    Unix.rename
      (Filename.concat denied "rename-source")
      (Filename.concat denied "rename-target"));
  expect_denied (fun () ->
    Unix.rename
      (Filename.concat allowed "reparent-source")
      (Filename.concat denied "reparent-target"));
  expect_denied (fun () -> Unix.rmdir (Filename.concat denied "remove-dir"));
  expect_denied (fun () -> Unix.mkdir (Filename.concat denied "mkdir") 0o700)
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; allowed; denied ] ->
    create allowed;
    expect_denied (fun () -> create denied)
  | [ _; "write-three"; allowed1; allowed2; allowed3; denied ] ->
    List.iter create [ allowed1; allowed2; allowed3 ];
    expect_denied (fun () -> create denied)
  | [ _; "access-rights"; allowed; denied ] -> access_rights allowed denied
  | _ -> exit 2
;;
