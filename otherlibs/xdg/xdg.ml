type t =
  { env : string -> string option
  ; win32 : bool
  ; home_dir : string
  ; mutable cache_dir : string
  ; mutable config_dir : string
  ; mutable data_dir : string
  ; mutable runtime_dir : string option
  }

let ( / ) = Filename.concat

type known_folder =
  | InternetCache
  | LocalAppData

external get_known_folder_path : known_folder -> string option
  = "dune_xdg__get_known_folder_path"

let make t env_var unix_default win32_folder =
  let default =
    if t.win32 then
      match get_known_folder_path win32_folder with
      | None -> ""
      | Some s -> s
    else unix_default
  in
  match t.env env_var with
  | None -> default
  | Some s when Filename.is_relative s -> default
  | Some s -> s

let cache_dir t =
  let home = t.home_dir in
  make t "XDG_CACHE_HOME" (home / ".cache") InternetCache

let config_dir t =
  let home = t.home_dir in
  make t "XDG_CONFIG_HOME" (home / ".config") LocalAppData

let data_dir t =
  let home = t.home_dir in
  make t "XDG_DATA_HOME" (home / ".local" / "share") LocalAppData

let create ?win32 ~env () =
  let win32 =
    match win32 with
    | None -> Sys.win32
    | Some s -> s
  in
  let home_dir =
    let var = if win32 then "USERPROFILE" else "HOME" in
    match env var with
    | None -> ""
    | Some s -> s
  in
  let t =
    { env
    ; win32
    ; home_dir
    ; cache_dir = ""
    ; config_dir = ""
    ; data_dir = ""
    ; runtime_dir = None
    }
  in
  t.cache_dir <- cache_dir t;
  t.config_dir <- config_dir t;
  t.data_dir <- data_dir t;
  t.runtime_dir <- env "XDG_RUNTIME_DIR";
  t

let home_dir t = t.home_dir

let config_dir t = t.config_dir

let data_dir t = t.data_dir

let cache_dir t = t.cache_dir

let runtime_dir t = t.runtime_dir
