(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Base = struct
  include OpamStd.AbstractString

  let compare = String.compare
  let equal = String.equal

  let check_suffix filename s =
    Filename.check_suffix filename s

  let add_extension filename suffix =
    filename ^ "." ^ suffix
end

let log fmt = OpamConsole.log "FILENAME" fmt
let slog = OpamConsole.slog

module Dir = struct

  include OpamStd.AbstractString

  let compare = String.compare
  let equal = String.equal

  let of_string dirname =
    let dirname =
      if dirname = "~" then OpamStd.Sys.home ()
      else if
        OpamStd.String.starts_with ~prefix:("~"^Filename.dir_sep) dirname
      then
        Filename.concat (OpamStd.Sys.home ())
          (OpamStd.String.remove_prefix ~prefix:("~"^Filename.dir_sep) dirname)
      else dirname
    in
    OpamSystem.real_path (OpamSystem.forward_to_back dirname)

  let to_string dirname = dirname

end

let raw_dir s = s

let mk_tmp_dir () =
  Dir.of_string @@ OpamSystem.mk_temp_dir ()

let with_tmp_dir fn =
  OpamSystem.with_tmp_dir (fun dir -> fn (Dir.of_string dir))

let with_tmp_dir_job fjob =
  OpamSystem.with_tmp_dir_job (fun dir -> fjob (Dir.of_string dir))

let rmdir dirname =
  OpamSystem.remove_dir (Dir.to_string dirname)

let rec rmdir_cleanup dirname =
  let sd = Dir.to_string dirname in
  if OpamSystem.dir_is_empty sd then (
    rmdir dirname;
    let parent = Filename.dirname sd in
    if parent <> sd then rmdir_cleanup parent
  )

let cwd () =
  Dir.of_string (Unix.getcwd ())

let mkdir dirname =
  OpamSystem.mkdir (Dir.to_string dirname)

let exists_dir dirname =
  try (Unix.stat (Dir.to_string dirname)).Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error _ -> false

let cleandir dirname =
  if exists_dir dirname then
    (log "cleandir %a" (slog Dir.to_string) dirname;
     OpamSystem.remove (Dir.to_string dirname);
     mkdir dirname)

let rec_dirs d =
  let fs = OpamSystem.rec_dirs (Dir.to_string d) in
  List.rev (List.rev_map Dir.of_string fs)

let dirs d =
  let fs = OpamSystem.dirs (Dir.to_string d) in
  List.rev (List.rev_map Dir.of_string fs)

let dir_is_empty d =
  OpamSystem.dir_is_empty (Dir.to_string d)

let in_dir dirname fn = OpamSystem.in_dir dirname fn

let env_of_list l = Array.of_list (List.rev_map (fun (k,v) -> k^"="^v) l)

let exec dirname ?env ?name ?metadata ?keep_going cmds =
  let env = match env with
    | None   -> None
    | Some l -> Some (env_of_list l) in
  in_dir dirname
    (fun () -> OpamSystem.commands ?env ?name ?metadata ?keep_going cmds)

let move_dir ~src ~dst =
  OpamSystem.mv (Dir.to_string src) (Dir.to_string dst)

let opt_dir dirname =
  if exists_dir dirname then Some dirname else None

let basename_dir dirname =
  Base.of_string (Filename.basename (Dir.to_string dirname))

let dirname_dir dirname = Filename.dirname (Dir.to_string dirname)

let link_dir ~target ~link =
  if exists_dir link then
    OpamSystem.internal_error "Cannot link: %s already exists."
      (Dir.to_string link)
  else
    OpamSystem.link (Dir.to_string target) (Dir.to_string link)

let to_list_dir dir =
  let base d = Dir.of_string (Filename.basename (Dir.to_string d)) in
  let rec aux acc dir =
    let d = dirname_dir dir in
    if d <> dir then aux (base dir :: acc) d
    else base dir :: acc in
  aux [] dir

let (/) d1 s2 =
  let s1 = Dir.to_string d1 in
  raw_dir (Filename.concat s1 s2)

let concat_and_resolve d1 s2 =
  let s1 = Dir.to_string d1 in
  Dir.of_string (Filename.concat s1 s2)

type t = {
  dirname:  Dir.t;
  basename: Base.t;
}

let create dirname basename =
  let b1 = OpamSystem.forward_to_back (Filename.dirname (Base.to_string basename)) in
  let b2 = Base.of_string (Filename.basename (Base.to_string basename)) in
  let dirname = OpamSystem.forward_to_back dirname in
  if basename = b2 then
    { dirname; basename }
  else
    { dirname = dirname / b1; basename = b2 }

let of_basename basename =
  let dirname = Dir.of_string Filename.current_dir_name in
  { dirname; basename }

let raw str =
  let dirname = raw_dir (Filename.dirname str) in
  let basename = Base.of_string (Filename.basename str) in
  create dirname basename

let to_string t =
  Filename.concat (Dir.to_string t.dirname) (Base.to_string t.basename)

let touch t =
  OpamSystem.write (to_string t) ""

let chmod t p =
  Unix.chmod (to_string t) p

let written_since file =
  let last_update =
    (Unix.stat (to_string file)).Unix.st_mtime
  in
  (Unix.time () -. last_update)

let of_string s =
  let dirname = Filename.dirname s in
  let basename = Filename.basename s in
  {
    dirname  = Dir.of_string dirname;
    basename = Base.of_string basename;
  }

let dirname t = t.dirname

let basename t = t.basename

let read filename =
  OpamSystem.read (to_string filename)

let open_in filename =
  try open_in (to_string filename)
  with Sys_error _ -> raise (OpamSystem.File_not_found (to_string filename))

let open_in_bin filename =
  try open_in_bin (to_string filename)
  with Sys_error _ -> raise (OpamSystem.File_not_found (to_string filename))

let open_out filename =
  try open_out (to_string filename)
  with Sys_error _ -> raise (OpamSystem.File_not_found (to_string filename))

let open_out_bin filename =
  try open_out_bin (to_string filename)
  with Sys_error _ -> raise (OpamSystem.File_not_found (to_string filename))

let write filename raw =
  OpamSystem.write (to_string filename) raw

let remove filename =
  OpamSystem.remove_file (to_string filename)

let with_open_out_bin_aux open_out_bin filename f =
  let v, oc =
    mkdir (dirname filename);
    try open_out_bin filename
    with Sys_error _ -> raise (OpamSystem.File_not_found (to_string filename))
  in
  try
    Unix.lockf (Unix.descr_of_out_channel oc) Unix.F_LOCK 0;
    f oc;
    close_out oc;
    v
  with e ->
    OpamStd.Exn.finalise e @@ fun () ->
    close_out oc; remove filename

let with_open_out_bin =
  with_open_out_bin_aux (fun f -> (), open_out_bin f)

let with_open_out_bin_atomic filename f =
  let open_temp_file filename =
    let mode = [Open_binary] in
    let perms = 0o666 in
    let temp_dir = Dir.to_string (dirname filename) in
    Filename.open_temp_file ~mode ~perms ~temp_dir "opam-atomic" ".tmp"
  in
  let temp_file = with_open_out_bin_aux open_temp_file filename f in
  try
    Sys.rename temp_file (to_string filename)
  with Sys_error _ ->
    OpamSystem.remove_file temp_file;
    raise (OpamSystem.File_not_found (to_string filename))

let exists filename =
  try (Unix.stat (to_string filename)).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error _ -> false

let opt_file filename =
  if exists filename then Some filename else None

let with_contents fn filename =
  fn (read filename)

let check_suffix filename s =
  Filename.check_suffix (to_string filename) s

let add_extension filename suffix =
  of_string ((to_string filename) ^ "." ^ suffix)

let chop_extension filename =
  of_string (Filename.chop_extension (to_string filename))

let rec_files d =
  let fs = OpamSystem.rec_files (Dir.to_string d) in
  List.rev_map of_string fs

let files d =
  let fs = OpamSystem.files (Dir.to_string d) in
  List.rev_map of_string fs

let files_and_links d =
  let fs = OpamSystem.files_all_not_dir (Dir.to_string d) in
  List.rev_map of_string fs

let copy ~src ~dst =
  if src <> dst then OpamSystem.copy_file (to_string src) (to_string dst)

let copy_dir ~src ~dst =
  if src <> dst then OpamSystem.copy_dir (Dir.to_string src) (Dir.to_string dst)

let install ?warning ?exec ~src ~dst () =
  if src <> dst then OpamSystem.install ?warning ?exec (to_string src) (to_string dst)

let move ~src ~dst =
  if src <> dst then
    OpamSystem.mv (to_string src) (to_string dst)

let readlink src =
  if exists src then
    try
      let rl = Unix.readlink (to_string src) in
      if Filename.is_relative rl then
        of_string (Filename.concat (dirname src) rl)
      else of_string rl
    with Unix.Unix_error _ -> src
  else
    OpamSystem.internal_error "%s does not exist." (to_string src)

let is_symlink src =
  try
    let s = Unix.lstat (to_string src) in
    s.Unix.st_kind = Unix.S_LNK
  with Unix.Unix_error _ -> false

let is_symlink_dir src =
  try
    let s = Unix.lstat (Dir.to_string src) in
    s.Unix.st_kind = Unix.S_LNK
  with Unix.Unix_error _ -> false

let is_exec file =
  try OpamSystem.is_exec (to_string file)
  with Unix.Unix_error _ ->
    OpamSystem.internal_error "%s does not exist." (to_string file)

let starts_with dirname filename =
  OpamStd.String.starts_with ~prefix:(Dir.to_string dirname) (to_string filename)

let dir_starts_with pfx dir =
  OpamStd.String.starts_with ~prefix:(Dir.to_string pfx) (Dir.to_string dir)

let remove_prefix prefix filename =
  let prefix =
    let str = Dir.to_string prefix in
    if str = "" then "" else Filename.concat str "" in
  let filename = to_string filename in
  OpamStd.String.remove_prefix ~prefix filename

let remove_prefix_dir prefix dir =
  let prefix = Dir.to_string prefix in
  let dirname = Dir.to_string dir in
  if prefix = "" then dirname
  else
    OpamStd.String.remove_prefix ~prefix dirname |>
    OpamStd.String.remove_prefix ~prefix:Filename.dir_sep

let process_in ?root fn src dst =
  let basename = match root with
    | None   -> basename src
    | Some r ->
      if starts_with r src then remove_prefix r src
      else OpamSystem.internal_error "%s is not a prefix of %s"
          (Dir.to_string r) (to_string src) in
  let dst = Filename.concat (Dir.to_string dst) basename in
  fn ~src ~dst:(of_string dst)

let copy_in ?root = process_in ?root copy

let is_archive filename =
  OpamSystem.is_archive (to_string filename)

let extract filename dirname =
  OpamSystem.extract (to_string filename) ~dir:(Dir.to_string dirname)

let extract_job filename dirname =
  OpamSystem.extract_job (to_string filename) ~dir:(Dir.to_string dirname)

let extract_in filename dirname =
  OpamSystem.extract_in (to_string filename) ~dir:(Dir.to_string dirname)

let extract_in_job filename dirname =
  OpamSystem.extract_in_job (to_string filename) ~dir:(Dir.to_string dirname)

let make_tar_gz_job filename dirname =
  OpamSystem.make_tar_gz_job (to_string filename) ~dir:(Dir.to_string dirname)

type generic_file =
  | D of Dir.t
  | F of t

let extract_generic_file filename dirname =
  match filename with
  | F f ->
    log "extracting %a to %a"
      (slog to_string) f
      (slog Dir.to_string) dirname;
    extract f dirname
  | D d ->
    if d <> dirname then (
      log "copying %a to %a"
        (slog Dir.to_string) d
        (slog Dir.to_string) dirname;
      copy_dir ~src:d ~dst:dirname
    )

let ends_with suffix filename =
  OpamStd.String.ends_with ~suffix (to_string filename)

let dir_ends_with suffix dirname =
  OpamStd.String.ends_with ~suffix (Dir.to_string dirname)

let remove_suffix suffix filename =
  let suffix = Base.to_string suffix in
  let filename = to_string filename in
  OpamStd.String.remove_suffix ~suffix filename

let rec find_in_parents f dir =
  if f dir then Some dir else
  let parent = dirname_dir dir in
  if parent = dir then None
  else find_in_parents f parent

let link ?(relative=false) ~target ~link =
  if target = link then () else
  let target =
    if not relative then to_string target else
    match
      find_in_parents (fun d -> d <> "/" && starts_with d link) (dirname target)
    with
    | None -> to_string target
    | Some ancestor ->
      let back =
        let rel = remove_prefix_dir ancestor (dirname link) in
        OpamStd.List.concat_map Filename.dir_sep
          (fun _ -> "..")
          (OpamStd.String.split rel Filename.dir_sep.[0])
      in
      let forward = remove_prefix ancestor target in
      Filename.concat back forward
  in
  OpamSystem.link target (to_string link)
[@@ocaml.warning "-16"]

let patch ?preprocess filename dirname =
  OpamSystem.patch ?preprocess ~dir:(Dir.to_string dirname) (to_string filename)

let flock flag ?dontblock file = OpamSystem.flock flag ?dontblock (to_string file)

let with_flock flag ?dontblock file f =
  let lock = OpamSystem.flock flag ?dontblock (to_string file) in
  try
    let (fd, ch) =
      match OpamSystem.get_lock_fd lock with
      | exception Not_found ->
        let null =
          if OpamStd.Sys.(os () = Win32) then
            "nul"
          else
            "/dev/null"
        in
        let ch = Stdlib.open_out null in
        Unix.descr_of_out_channel ch, Some ch
      | fd ->
        fd, None
    in
    let r = f fd in
    OpamSystem.funlock lock;
    OpamStd.Option.iter Stdlib.close_out ch;
    r
  with e ->
    OpamStd.Exn.finalise e @@ fun () ->
    OpamSystem.funlock lock

let with_flock_upgrade flag ?dontblock lock f =
  if OpamSystem.lock_isatleast flag lock then f (OpamSystem.get_lock_fd lock)
  else (
    let old_flag = OpamSystem.get_lock_flag lock in
    OpamSystem.flock_update flag ?dontblock lock;
    try
      let r = f (OpamSystem.get_lock_fd lock) in
      OpamSystem.flock_update old_flag lock;
      r
    with e ->
      OpamStd.Exn.finalise e @@ fun () ->
      OpamSystem.flock_update old_flag lock
  )

let with_flock_write_then_read ?dontblock file write read =
  let lock = OpamSystem.flock `Lock_write ?dontblock (to_string file) in
  try
    let r = write (OpamSystem.get_lock_fd lock) in
    OpamSystem.flock_update `Lock_read lock;
    let r = read r in
    OpamSystem.funlock lock;
    r
  with e ->
    OpamStd.Exn.finalise e @@ fun () ->
    OpamSystem.funlock lock

let prettify_path s =
  let aux ~short ~prefix =
    let prefix = Filename.concat prefix "" in
    if OpamStd.String.starts_with ~prefix s then
      let suffix = OpamStd.String.remove_prefix ~prefix s in
      Some (Filename.concat short suffix)
    else
      None in
  try
    match aux ~short:"~" ~prefix:(OpamStd.Sys.home ()) with
    | Some p -> p
    | None   -> s
  with Not_found -> s

let prettify_dir d =
  prettify_path (Dir.to_string d)

let prettify s =
  prettify_path (to_string s)

let to_json x = `String (to_string x)
let of_json = function
  | `String x -> (try Some (of_string x) with _ -> None)
  | _ -> None

let compare {dirname; basename} f =
  let dir = Dir.compare dirname f.dirname in
  if dir <> 0 then dir else
    Base.compare basename f.basename

let equal f g = compare f g = 0

module O = struct
  type tmp = t
  type t = tmp
  let compare = compare
  let to_string = to_string
  let to_json = to_json
  let of_json = of_json
end

module Map = OpamStd.Map.Make(O)
module Set = OpamStd.Set.Make(O)

module SubPath = struct

  include OpamStd.AbstractString

  let compare = String.compare
  let equal = String.equal

  let of_string s =
    OpamSystem.back_to_forward s
    |> OpamStd.String.remove_prefix ~prefix:"./"
    |> of_string
  let to_string = OpamSystem.forward_to_back
  let normalised_string s = s

  let (/) d s = d / to_string s
  let (/?) d = function
    | None -> d
    | Some s -> d / to_string s

end

module Op = struct

  let (/) = (/)

  let (//) d1 s2 =
    let d = Filename.dirname s2 in
    let b = Filename.basename s2 in
    if d <> "." then
      create (d1 / d) (Base.of_string b)
    else
      create d1 (Base.of_string s2)

end

module Attribute = struct

  type t = {
    base: Base.t;
    md5 : OpamHash.t;
    perm: int option;
  }

  let base t = t.base

  let md5 t = t.md5

  let perm t = t.perm

  let create base md5 perm =
    { base; md5; perm=perm }

  let to_string_list t =
    let perm = match t.perm with
      | None   -> []
      | Some p -> [Printf.sprintf "0o%o" p] in
    Base.to_string t.base :: OpamHash.to_string t.md5 :: perm

  let of_string_list = function
    | [base; md5]      ->
      { base=Base.of_string base; md5=OpamHash.of_string md5; perm=None }
    | [base;md5; perm] ->
      { base=Base.of_string base;
        md5=OpamHash.of_string md5;
        perm=Some (int_of_string perm) }
    | k                -> OpamSystem.internal_error
                            "remote_file: '%s' is not a valid line."
                            (String.concat " " k)

  let to_string t = String.concat " " (to_string_list t)
  let of_string s = of_string_list (OpamStd.String.split s ' ')

  let to_json x =
    `O ([ ("base" , Base.to_json x.base);
          ("md5"  , `String (OpamHash.to_string x.md5))]
        @ match x. perm with
          | None   -> []
          | Some p -> ["perm", `String (string_of_int p)])

  let of_json = function
    | `O dict ->
      begin try
          let open OpamStd.Option.Op in
          Base.of_json (OpamStd.List.assoc String.equal "base" dict)
          >>= fun base ->
          OpamHash.of_json (OpamStd.List.assoc String.equal "md5" dict)
          >>= fun md5 ->
          let perm =
            if not (OpamStd.List.mem_assoc String.equal "perm" dict) then None
            else match OpamStd.List.assoc String.equal "perm" dict with
              | `String hash ->
                (try Some (int_of_string hash) with _ -> raise Not_found)
              | _ -> raise Not_found
          in
          Some { base; md5; perm }
        with Not_found -> None
      end
    | _ -> None

  let compare {base; md5; perm} a =
    let base = Base.compare base a.base in
    if base <> 0 then base else
    let md5 = OpamHash.compare md5 a.md5 in
    if md5 <> 0 then md5 else
      OpamStd.Option.compare Int.compare perm a.perm

  let equal a b = compare a b = 0

  module O = struct
    type tmp = t
    type t = tmp
    let to_string = to_string
    let compare = compare
    let to_json = to_json
    let of_json = of_json
  end

  module Set = OpamStd.Set.Make(O)

  module Map = OpamStd.Map.Make(O)

end

let to_attribute root file =
  let basename = Base.of_string (remove_prefix root file) in
  let perm =
    let s = Unix.stat (to_string file) in
    s.Unix.st_perm in
  let digest = OpamHash.compute ~kind:`MD5 (to_string file) in
  Attribute.create basename digest (Some perm)
