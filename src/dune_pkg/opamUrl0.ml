include OpamUrl
open Stdune

module T = struct
  type nonrec t = t

  let to_dyn t = Dyn.string (OpamUrl.to_string t)
  let compare x y = Ordering.of_int (OpamUrl.compare x y)
end

include T

let decode_loc =
  let open Dune_sexp.Decoder in
  map_validate (located string) ~f:(fun (loc, s) ->
    match OpamUrl.of_string s with
    | url -> Ok (loc, url)
    | exception OpamUrl.Parse_error m ->
      Error (User_message.make [ Pp.text "invalid url "; Pp.text m ]))
;;

let rev t = t.hash
let hash = Poly.hash

let is_version_control t =
  match t.backend with
  | #version_control -> true
  | _ -> false
;;

let is_local t = String.equal t.transport "file"

let local_or_git_only url loc =
  match (url : t).backend with
  | `rsync -> `Path (Path.of_string url.path)
  | `git -> `Git
  | `http | `darcs | `hg ->
    User_error.raise
      ~loc
      ~hints:[ Pp.text "Specify either a file path or git repo via SSH/HTTPS" ]
      [ Pp.textf "Could not determine location of repository %s" @@ OpamUrl.to_string url
      ]
;;

include Comparable.Make (T)

let find_revision t rev_store =
  let url = base_url t in
  let rev = rev t in
  let not_found () =
    Error
      (User_message.make
         [ (match rev with
            | None -> Pp.textf "default branch not found in %s" url
            | Some rev -> Pp.textf "revision %S not found in %s" rev url)
         ])
  in
  let open Fiber.O in
  let remote = Rev_store.remote rev_store ~url in
  let* rev =
    match
      match rev with
      | None -> `Default_branch
      | Some revision ->
        (match Rev_store.Object.of_sha1 revision with
         | Some sha1 -> `Object sha1
         | None -> `Ref revision)
    with
    | `Default_branch ->
      Rev_store.Remote.default_branch remote
      >>| (function
       | Some s -> Ok (`Resolved s)
       | None ->
         Error
           (User_message.make
              [ Pp.textf
                  "no revision specified in %S and remote has no default branch"
                  (to_string t)
              ]))
    | `Object obj -> Fiber.return (Ok (`Unresolved obj))
    | `Ref revision ->
      Rev_store.resolve_revision rev_store remote ~revision
      >>| (function
       | None -> not_found ()
       | Some o -> Ok (`Resolved o))
  in
  match rev with
  | Error e -> Fiber.return @@ Error e
  | Ok (`Resolved o) -> Rev_store.fetch_resolved rev_store remote o >>| Result.ok
  | Ok (`Unresolved o) ->
    Rev_store.fetch_object rev_store remote o
    >>| (function
     | None -> not_found ()
     | Some rev -> Ok rev)
;;
