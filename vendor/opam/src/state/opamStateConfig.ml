(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStateTypes

module E = struct

  type OpamStd.Config.E.t +=
    | BUILDDOC of bool option
    | BUILDTEST of bool option
    | DOWNLOADJOBS of int option
    | DRYRUN of bool option
    | IGNORECONSTRAINTS of string option
    | JOBS of int option
    | LOCKED of string option
    | MAKECMD of string option
    | NODEPEXTS of bool option
    | NOENVNOTICE of bool option
    | ROOT of string option
    | SWITCH of string option
    | UNLOCKBASE of bool option
    | WITHDEVSETUP of bool option
    | WITHDOC of bool option
    | WITHTEST of bool option

  open OpamStd.Config.E
  let builddoc = value (function BUILDDOC b -> b | _ -> None)
  let buildtest = value (function BUILDTEST b -> b | _ -> None)
  let downloadjobs = value (function DOWNLOADJOBS i -> i | _ -> None)
  let dryrun = value (function DRYRUN b -> b | _ -> None)
  let ignoreconstraints = value (function IGNORECONSTRAINTS s -> s | _ -> None)
  let jobs = value (function JOBS i -> i | _ -> None)
  let locked = value (function LOCKED s -> s | _ -> None)
  let makecmd = value (function MAKECMD s -> s | _ -> None)
  let nodepexts = value (function NODEPEXTS b -> b | _ -> None)
  let noenvnotice = value (function NOENVNOTICE b -> b | _ -> None)
  let root = value (function ROOT s -> s | _ -> None)
  let switch = value (function SWITCH s -> s | _ -> None)
  let unlockbase = value (function UNLOCKBASE b -> b | _ -> None)
  let withdevsetup = value (function WITHDEVSETUP b -> b | _ -> None)
  let withdoc = value (function WITHDOC b -> b | _ -> None)
  let withtest = value (function WITHTEST b -> b | _ -> None)

end

type t = {
  root_dir: OpamFilename.Dir.t;
  current_switch: OpamSwitch.t option;
  switch_from: provenance;
  jobs: int Lazy.t;
  dl_jobs: int;
  build_test: bool;
  build_doc: bool;
  dev_setup: bool;
  dryrun: bool;
  makecmd: string Lazy.t;
  ignore_constraints_on: name_set;
  unlock_base: bool;
  no_env_notice: bool;
  locked: string option;
  no_depexts: bool;
}

let default = {
  root_dir = (
    (* On Windows, if a .opam directory is found in %HOME% or %USERPROFILE% then
       then we'll use it. Otherwise, we use %LOCALAPPDATA%. *)
    let home_location =
      let open OpamFilename in
      concat_and_resolve (Dir.of_string (OpamStd.Sys.home ())) ".opam"
    in
    if not Sys.win32 || OpamFilename.exists_dir home_location then
      home_location
    else
      let open OpamFilename in
      let local_appdata =
        (* CSIDL_LOCAL_APPDATA = 0x1c *)
        Dir.of_string (OpamStubs.(shGetFolderPath 0x1c SHGFP_TYPE_CURRENT))
      in
      concat_and_resolve local_appdata "opam"
    );
  current_switch = None;
  switch_from = `Default;
  jobs = lazy (max 1 (OpamSysPoll.cores () - 1));
  dl_jobs = 3;
  build_test = false;
  build_doc = false;
  dev_setup = false;
  dryrun = false;
  makecmd = lazy OpamStd.Sys.(
      match os () with
      | FreeBSD | OpenBSD | NetBSD | DragonFly -> "gmake"
      | _ -> "make"
    );
  ignore_constraints_on = OpamPackage.Name.Set.empty;
  unlock_base = false;
  no_env_notice = false;
  locked = None;
  no_depexts = false;
}

type 'a options_fun =
  ?root_dir:OpamFilename.Dir.t ->
  ?current_switch:OpamSwitch.t ->
  ?switch_from:provenance ->
  ?jobs:(int Lazy.t) ->
  ?dl_jobs:int ->
  ?build_test:bool ->
  ?build_doc:bool ->
  ?dev_setup:bool ->
  ?dryrun:bool ->
  ?makecmd:string Lazy.t ->
  ?ignore_constraints_on:name_set ->
  ?unlock_base:bool ->
  ?no_env_notice:bool ->
  ?locked:string option ->
  ?no_depexts: bool ->
  'a

let setk k t
    ?root_dir
    ?current_switch
    ?switch_from
    ?jobs
    ?dl_jobs
    ?build_test
    ?build_doc
    ?dev_setup
    ?dryrun
    ?makecmd
    ?ignore_constraints_on
    ?unlock_base
    ?no_env_notice
    ?locked
    ?no_depexts
  =
  let (+) x opt = match opt with Some x -> x | None -> x in
  k {
    root_dir = t.root_dir + root_dir;
    current_switch =
      (match current_switch with None -> t.current_switch | s -> s);
    switch_from = t.switch_from + switch_from;
    jobs = t.jobs + jobs;
    dl_jobs = t.dl_jobs + dl_jobs;
    build_test = t.build_test + build_test;
    build_doc = t.build_doc + build_doc;
    dev_setup = t.dev_setup + dev_setup;
    dryrun = t.dryrun + dryrun;
    makecmd = t.makecmd + makecmd;
    ignore_constraints_on = t.ignore_constraints_on + ignore_constraints_on;
    unlock_base = t.unlock_base + unlock_base;
    no_env_notice = t.no_env_notice + no_env_notice;
    locked = t.locked + locked;
    no_depexts = t.no_depexts + no_depexts;
  }

let set t = setk (fun x () -> x) t

let r = ref default

let update ?noop:_ = setk (fun cfg () -> r := cfg) !r

let initk k =
  let open OpamStd.Option.Op in
  let current_switch, switch_from =
    match E.switch () with
    | Some "" | None -> None, None
    | Some s -> Some (OpamSwitch.of_string s), Some `Env
  in
  setk (setk (fun c -> r := c; k)) !r
    ?root_dir:(E.root () >>| OpamFilename.Dir.of_string)
    ?current_switch
    ?switch_from
    ?jobs:(E.jobs () >>| fun s -> lazy s)
    ?dl_jobs:(E.downloadjobs ())
    ?build_test:(E.withtest () ++ E.buildtest ())
    ?build_doc:(E.withdoc () ++ E.builddoc ())
    ?dev_setup:(E.withdevsetup())
    ?dryrun:(E.dryrun ())
    ?makecmd:(E.makecmd () >>| fun s -> lazy s)
    ?ignore_constraints_on:
      (E.ignoreconstraints () >>| fun s ->
       OpamStd.String.split s ',' |>
       List.map OpamPackage.Name.of_string |>
       OpamPackage.Name.Set.of_list)
    ?unlock_base:(E.unlockbase ())
    ?no_env_notice:(E.noenvnotice ())
    ?locked:(E.locked () >>| function "" -> None | s -> Some s)
    ?no_depexts:(E.nodepexts ())

let init ?noop:_ = initk (fun () -> ())

let opamroot ?root_dir () =
  let open OpamStd.Option.Op in
  (root_dir >>+ fun () ->
   OpamStd.Env.getopt "OPAMROOT" >>| OpamFilename.Dir.of_string)
  +! default.root_dir

let is_newer_raw = function
  | Some v ->
    OpamVersion.compare v OpamFile.Config.root_version > 0
  | None -> false

let is_newer config =
    is_newer_raw (Some (OpamFile.Config.opam_root_version config))

(** none -> shouldn't load (write attempt in readonly)
    Some true -> everything is fine normal read
    Some false -> readonly accorded, load with best effort *)
let is_readonly_opamroot_raw ?(lock_kind=`Lock_write) version =
  let newer = is_newer_raw version in
  let write = lock_kind = `Lock_write in
  if newer && write then None else
    Some (newer && not write)

let is_readonly_opamroot_t ?lock_kind gt =
  is_readonly_opamroot_raw ?lock_kind
    (Some (OpamFile.Config.opam_root_version gt.config))

let is_newer_than_self ?lock_kind gt =
  is_readonly_opamroot_t ?lock_kind gt <> Some false

let load_if_possible_raw ?lock_kind root version (read,read_wo_err) f =
  match is_readonly_opamroot_raw ?lock_kind version with
  | None ->
    OpamConsole.error_and_exit `Locked
      "Refusing write access to %s, which is more recent than this version of \
       opam (%s > %s), aborting."
      (OpamFilename.Dir.to_string root)
      (OpamStd.Option.to_string OpamVersion.to_string version)
      OpamVersion.(to_string OpamFile.Config.root_version)
  | Some true -> read_wo_err f
  | Some false -> read f

let load_if_possible_t ?lock_kind opamroot config readf f =
  load_if_possible_raw ?lock_kind
    opamroot (Some (OpamFile.Config.opam_root_version config)) readf f

let load_if_possible ?lock_kind gt =
  load_if_possible_t ?lock_kind gt.root gt.config

let load_config_root ?lock_kind readf opamroot =
  let f = OpamPath.config opamroot in
  load_if_possible_raw ?lock_kind
    opamroot
    (OpamFile.Config.raw_root_version f)
    readf f

let safe read read' default =
  let safe r f = OpamStd.Option.default default @@ r f in
  safe read, safe read'

let safe_load ?lock_kind opamroot =
  load_config_root ?lock_kind
    OpamFile.Config.(safe read_opt BestEffort.read_opt empty) opamroot

let load ?lock_kind opamroot =
  load_config_root ?lock_kind
    OpamFile.Config.(read_opt, BestEffort.read_opt) opamroot

(* switches *)
module Switch = struct

  let load_raw ?lock_kind root config readf switch =
    load_if_possible_t ?lock_kind root config readf
      (OpamPath.Switch.switch_config root switch)

  let safe_load_t ?lock_kind root switch =
    let config = safe_load ~lock_kind:`Lock_read root in
    load_raw ?lock_kind root config
      OpamFile.Switch_config.(safe read_opt BestEffort.read_opt empty)
      switch

  let load ?lock_kind gt readf switch =
    load_raw ?lock_kind gt.root gt.config readf switch

  let safe_load ?lock_kind gt switch =
    load ?lock_kind gt
      OpamFile.Switch_config.(safe read_opt BestEffort.read_opt empty)
      switch

  let read_opt ?lock_kind gt switch =
    load ?lock_kind gt
      OpamFile.Switch_config.(read_opt, BestEffort.read_opt)
      switch

  let safe_read_selections ?lock_kind gt switch =
    load_if_possible ?lock_kind gt
      OpamFile.SwitchSelections.(safe read_opt BestEffort.read_opt empty)
      (OpamPath.Switch.selections gt.root switch)

end

(* repos *)
module Repos = struct
  let safe_read ?lock_kind gt =
    load_if_possible ?lock_kind gt
      OpamFile.Repos_config.(safe read_opt BestEffort.read_opt empty)
      (OpamPath.repos_config gt.root)
end

let downgrade_2_1_switch f =
  let filename = OpamFile.filename f in
  let str_f = OpamFilename.to_string filename in
  let opamfile = OpamParser.FullPos.file str_f in
  let opamfile' =
    let open OpamParserTypes.FullPos in
    { opamfile with
      file_contents =
        List.map (fun item ->
            match item.pelem with
            | Variable (({pelem = "opam-version"; _} as opam_v),
                        ({pelem = String "2.1"; _} as v)) ->
              { item with
                pelem = Variable ({opam_v with pelem = "opam-version"},
                                  {v with pelem = String "2.0"})}
            | _ -> item)
          opamfile.file_contents}
  in
  if opamfile' = opamfile then None else
    Some (opamfile'
          |> OpamPrinter.FullPos.opamfile
          |> OpamFile.Switch_config.read_from_string)

let local_switch_exists root switch =
  (* we don't use safe loading function to avoid errors displaying *)
  let f = OpamPath.Switch.switch_config root switch in
  match OpamFile.Switch_config.BestEffort.read_opt f with
  | None -> false
  | Some conf -> conf.OpamFile.Switch_config.opam_root = Some root
  | exception (OpamPp.Bad_version _ as e) ->
    match OpamFile.Config.raw_root_version (OpamPath.config root) with
    | None -> raise e
    | Some _ ->
      match downgrade_2_1_switch f with
      | None -> raise e
      | Some conf ->
        if conf.OpamFile.Switch_config.opam_root = Some root then
          (OpamFile.Switch_config.write f conf; true)
        else false

let resolve_local_switch root s =
  let switch_root = OpamSwitch.get_root root s in
  if OpamSwitch.is_external s && OpamFilename.dirname_dir switch_root = root
  then OpamSwitch.of_string (OpamFilename.remove_prefix_dir root switch_root)
  else s

let get_current_switch_from_cwd root =
  try
    let open OpamStd.Option.Op in
    OpamFilename.find_in_parents (fun dir ->
        OpamSwitch.of_string (OpamFilename.Dir.to_string dir) |>
        local_switch_exists root)
      (OpamFilename.cwd ())
    >>| OpamSwitch.of_dirname
    >>| resolve_local_switch root
  with OpamPp.Bad_version _ -> None

(* do we want `load_defaults` to fail / run a format upgrade ? *)
let load_defaults ?lock_kind root_dir =
  let current_switch =
    match E.switch () with
    | Some "" | None -> get_current_switch_from_cwd root_dir
    | _ -> (* OPAMSWITCH is set, no need to lookup *) None
  in
  match try load ?lock_kind root_dir with OpamPp.Bad_version _ -> None with
  | None ->
    update ?current_switch ();
    None
  | Some conf ->
    let open OpamStd.Option.Op in
    OpamRepositoryConfig.update
      ?download_tool:(OpamFile.Config.dl_tool conf >>| function
        | (CString c,None)::_ as t
          when OpamStd.String.ends_with ~suffix:"curl" c -> lazy (t, `Curl)
        | t -> lazy (t, `Default))
      ~validation_hook:(OpamFile.Config.validation_hook conf)
      ();
    update
      ?current_switch:(OpamFile.Config.switch conf)
      ~switch_from:`Default
      ?jobs:(OpamFile.Config.jobs conf >>| fun s -> lazy s)
      ~dl_jobs:(OpamFile.Config.dl_jobs conf)
      ();
    update ?current_switch ();
    Some conf

let get_switch_opt () =
  match !r.current_switch with
  | Some s ->
    Some (resolve_local_switch !r.root_dir s)
  | None -> None

let get_switch () =
  match get_switch_opt () with
  | Some s -> s
  | None ->
    OpamConsole.error_and_exit `Configuration_error
      "No switch is currently set. Please use 'opam switch' to set or install \
       a switch"
