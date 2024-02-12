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
