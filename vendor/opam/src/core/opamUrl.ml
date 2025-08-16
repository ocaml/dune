(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStd.Op

type version_control = [ `git | `darcs | `hg ]

type backend = [ `http | `rsync | version_control ]

type t = {
  transport: string;
  path: string;
  hash: string option;
  backend: backend;
}

let empty = {
  backend = `http;
  transport = "https";
  path = "";
  hash = None;
}

let compare {transport; path; hash; backend} u =
  let transport = String.compare transport u.transport in
  if transport <> 0 then transport else
  let path = String.compare path u.path in
  if path <> 0 then path else
  let hash = OpamStd.Option.compare String.compare hash u.hash in
  if hash <> 0 then hash else
    compare backend u.backend

let equal u v = compare u v = 0

exception Parse_error of string
let parse_error s = raise (Parse_error s)

let split_url =
  let re =
    Re.(compile @@ whole_string @@ seq [
        (* Parse the scheme, which is either backend+protocol or just a protocol *)
        opt @@ seq [
          (* Backend *)
          opt @@ seq [ group @@ rep @@ diff any (set "+:");
                       alt [ char '+'; str "://"] ];
          (* Protocol *)
          group @@ rep @@ diff any (char ':');
          (* Separator *)
          str "://"
        ];
        (* Parse the path, with is either path or path.suffix (suffix contains no .) *)
        group @@ seq [
          non_greedy @@ rep @@ diff any (char '#');
          (* If there's a .suffix, group it separately (used for backend guessing) *)
          opt @@ seq [ char '.'; group @@ rep1 @@ diff any (set "\\/.#")]
        ];
        (* Parse the fragment (git branch, etc.) *)
        opt @@ seq [ char '#'; group @@ rep any ];
      ])
  in
  fun u ->
    match Re.Group.all (Re.exec re u) with
    | [| _; vc; transport; path; suffix; hash |] ->
      let opt = function "" -> None | s -> Some s in
      opt vc, opt transport, path, opt suffix, opt hash
    | _ -> assert false

let vc_of_string = function
  | "git" -> `git
  | "hg" -> `hg
  | "darcs" -> `darcs
  | vc ->
    parse_error (Printf.sprintf "unsupported version control system %s"
                (OpamConsole.colorise `underline vc))

let string_of_vc = function
  | `git   -> "git"
  | `darcs -> "darcs"
  | `hg    -> "hg"

let string_of_backend = function
  | `http  -> "http"
  | `rsync -> "rsync"
  | #version_control as vc -> string_of_vc vc

let backend_of_string = function
  | "http" | "https" | "ftp" | "wget" | "curl" -> `http
  | "file" -> `rsync
  | "git" -> `git
  | "darcs" -> `darcs
  | "hg" -> `hg
  | "path" | "local" | "rsync" | "ssh" | "scp" | "sftp" -> `rsync
  | p ->
    parse_error (Printf.sprintf "unsupported protocol %s"
                (OpamConsole.colorise `underline p))


let looks_like_ssh_path =
  (* ':' before any '/' : assume ssh, like git does. Exception for 'x:' with
     single char, because Windows *)
  let re =
    Re.(compile @@ seq [
        group @@ repn (diff any (set "/:")) 2 None;
        char ':';
        opt @@ char '/';
        opt @@ group @@ seq [
          alt [
            diff any digit;
            seq [rep digit; compl [digit; char '/']]
          ];
          rep any;
        ];
        eos;
      ])
  in
  fun path ->
    try
      let sub = Re.exec re path in
      Some (Re.Group.get sub 1 ^
            try "/" ^ Re.Group.get sub 2 with Not_found -> "")
    with Not_found -> None

let parse ?backend ?(handle_suffix=true) ?(from_file=true) s =
  let vc, transport, path, suffix, hash = split_url s in
  let backend =
    match backend with
    | Some b -> b
    | None ->
      match vc with
      | Some vc -> vc_of_string vc
      | None ->
        let of_suffix ~default =
          if not handle_suffix then default else
          match suffix with
          | Some sf -> (try vc_of_string sf with Parse_error _ -> default)
          | None ->
            match OpamStd.String.cut_at path '@' with
            | Some (user, _) ->
              (try vc_of_string user with Parse_error _ -> default)
            | None -> default
        in
        match transport with
        | None -> of_suffix ~default:`rsync
        | Some tr ->
          try vc_of_string tr with Parse_error _ ->
            of_suffix ~default:(backend_of_string tr)
  in
  let transport, path =
    match backend, transport, looks_like_ssh_path path with
    | `http, None, _ ->
      "http", path
    | _, (None | Some ("git"|"hg"|"darcs")), Some path ->
      "ssh", path
    | _, (None | Some ("hg"|"darcs")), None ->
      "file", OpamSystem.real_path path |> OpamSystem.back_to_forward
    | `rsync, Some "file", _ when not from_file ->
      "file", OpamSystem.real_path path |> OpamSystem.back_to_forward
    | _, Some tr, _ ->
      tr, path
  in
  {
    transport;
    path;
    hash;
    backend;
  }

let parse_opt ?(quiet=false) ?backend ?handle_suffix ?from_file s =
  try
    Some (parse ?backend ?handle_suffix ?from_file s)
  with Parse_error pe ->
    if not quiet then
      OpamConsole.warning "URL parsing error on %s: %s"
        (OpamConsole.colorise `underline s) pe;
    None

let of_string url = parse ~handle_suffix:false url

let to_string_t ?subpath url =
  let hash = match url.hash with Some h -> "#" ^ h | None -> "" in
  let subpath =
    match subpath with
    | Some sb ->
      Printf.sprintf "directory /%s in "
        (OpamFilename.SubPath.normalised_string sb)
    | None -> ""
  in
  match url.backend with
  | #version_control as vc ->
    let vc = string_of_backend vc in
    if url.transport = vc then (* Don't be redundant on e.g git:// protocols *)
      Printf.sprintf "%s%s://%s%s" subpath vc url.path hash
    else
      Printf.sprintf "%s%s+%s://%s%s" subpath vc url.transport url.path hash
  | `rsync | `http ->
    Printf.sprintf "%s%s://%s%s" subpath url.transport url.path hash

let to_string url = to_string_t url
let to_string_w_subpath subpath = to_string_t ?subpath

let base_url url =
  match url.transport with
  | "" -> url.path
  | tr -> Printf.sprintf "%s://%s" tr url.path

let local_path = function
  | { transport = ("file"|"path"|"local"|"rsync"); path;
      hash = _; backend = (#version_control | `rsync); }
    when looks_like_ssh_path path = None ->
    Some path
  | _ -> None

let local_dir url =
  let open OpamStd.Option.Op in
  local_path url >>|
  OpamFilename.Dir.of_string >>= fun d ->
  if OpamFilename.exists_dir d then Some d
  else None

let local_file url =
  let open OpamStd.Option.Op in
  local_path url >>|
  OpamFilename.of_string >>= fun f ->
  if OpamFilename.exists f then Some f
  else None

let guess_version_control s =
  let vc,transport,path,_,_ = split_url s in
  if vc = None && transport = None && looks_like_ssh_path path = None then
    let open OpamFilename in
    let open Op in
    let dir = Dir.of_string path in
    if exists_dir (dir / ".git") || exists (dir // ".git")
    then Some`git else
    if exists_dir (dir / ".hg") then Some `hg else
    if exists_dir (dir / "_darcs") then Some `darcs else
      None
  else
  None

let basename =
  let re =
    Re.(compile @@ seq [
        opt @@ seq [rep any; char '/'];
        group @@ rep1 (diff any (char '/'));
        rep @@ char '/';
      ])
  in
  fun t ->
    try
      Re.Group.get (Re.exec re t.path) 1
    with Not_found -> ""

let root =
  let re = Re.(compile @@ seq [char '/'; rep any]) in
  fun t ->
    let path =
      (* The special-casing of "file" is needed for Windows *)
      if t.transport = "file" then
        ""
      else
        Re.replace_string re ~by:"" t.path
    in
    { t with path}

let has_trailing_slash url =
  OpamStd.String.ends_with ~suffix:"/" url.path

let to_json url = `String (to_string url)
let of_json = function
| `String s -> (try Some (of_string s) with _ -> None)
| _ -> None

type url = t

let map_file_url f url =
  if url.transport = "file" then
    {url with path = f url.path}
  else
    url

module O = struct
  type t = url
  let to_string = to_string
  let to_json = to_json
  let of_json = of_json
  let compare = compare
end

module Set = OpamStd.Set.Make(O)

module Map = OpamStd.Map.Make(O)

module Op = struct

  (** appending to an url path *)
  let ( / ) url dir =
    let url =
      if Filename.is_relative dir then
        url
      else
        root url
    in
    (* Even on Windows, a file:// _should_ use slash *)
    let dir = OpamSystem.back_to_forward dir in
    let path =
      if has_trailing_slash url || url.path = "" then url.path ^ dir
      else url.path ^ "/" ^ dir
    in
    {url with path }

end
