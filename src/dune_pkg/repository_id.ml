open Import

type t = Git_hash of string

let equal a b =
  match a, b with
  | Git_hash a, Git_hash b -> String.equal a b
;;

let encode : t Encoder.t = function
  | Git_hash commitish ->
    List
      [ Dune_lang.atom_or_quoted_string "git_hash"
      ; Dune_lang.atom_or_quoted_string commitish
      ]
;;

let decode =
  let open Decoder in
  let+ constr, stamp = pair string string in
  match constr with
  | "git_hash" -> Git_hash stamp
  | _ -> failwith "TODO: parsing failure"
;;

let to_dyn = function
  | Git_hash commitish -> Dyn.Variant ("Git_hash", [ String commitish ])
;;

let git_hash = function
  | Git_hash committish -> Some committish
;;

let of_git_hash v = Git_hash v

let attempt_repo_id ~dir =
  let head_path = Path.append_local dir (Path.Local.of_string "HEAD") in
  match Path.lstat head_path with
  | Ok { st_kind = S_REG; _ } ->
    let head = Io.file_line head_path 0 in
    (match String.lsplit2 head ~on:' ' with
     | None ->
       (* when checking out a commit *)
       Some (Git_hash head)
     | Some ("ref:", reference) ->
       (* when a reference is checked out (e.g. a branch) *)
       let reference = Path.Local.of_string reference in
       let rev_path = Path.append_local dir reference in
       let commit_id = Io.file_line rev_path 0 in
       Some (Git_hash commit_id)
     | Some _ -> None)
  | Ok _ | Error _ -> None
;;

let repo_id_of_git dir =
  let res = attempt_repo_id ~dir in
  match res with
  | Some _ as v -> v
  | None ->
    (* if it is not a bare repo the repo is in .git, try again here *)
    let git_dir = Path.Local.of_string ".git" in
    let dir = Path.append_local dir git_dir in
    attempt_repo_id ~dir
;;

let of_path dir =
  match Path.stat dir with
  | Ok { st_kind = S_DIR; _ } -> repo_id_of_git dir
  | Ok _ | Error _ -> None
;;
