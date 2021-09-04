type t =
  { env : (string * string) list
  ; win32 : bool
  ; home_dir : string
  ; mutable cache_dir : string
  ; mutable config_dir : string
  ; mutable data_dir : string
  ; mutable runtime_dir : string option
  }

let ( / ) = Filename.concat

let make t env_var unix_default win32_default =
  match List.assoc_opt env_var t.env with
  | Some s -> s
  | None ->
    if t.win32 then
      win32_default
    else
      unix_default

let cache_dir t =
  let home = t.home_dir in
  make t "XDG_CACHE_HOME" (home / ".cache") (home / "Local Settings" / "Cache")

let config_dir t =
  let home = t.home_dir in
  make t "XDG_CONFIG_HOME" (home / ".config") (home / "Local Settings")

let data_dir t =
  let home = t.home_dir in
  make t "XDG_DATA_HOME"
    (home / ".local" / "share")
    (match List.assoc_opt "AppData" t.env with
    | Some s -> s
    | None -> "")

let of_assoc ?win32 env =
  let win32 =
    match win32 with
    | None -> Sys.win32
    | Some s -> s
  in
  let home_dir =
    match List.assoc_opt "HOME" env with
    | Some s -> s
    | None ->
      if win32 then
        match List.assoc_opt "AppData" env with
        | None -> ""
        | Some s -> s
      else
        ""
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
  t.runtime_dir <- List.assoc_opt "XDG_RUNTIME_DIR" env;
  t

let home_dir t = t.home_dir

let config_dir t = t.config_dir

let data_dir t = t.data_dir

let cache_dir t = t.cache_dir

let runtime_dir t = t.runtime_dir
