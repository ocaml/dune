(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

include OpamStubsTypes

let that's_a_no_no _ = failwith "Windows only. This function isn't implemented."

let getCurrentProcessID = that's_a_no_no
let getpid = Unix.getpid
let getStdHandle = that's_a_no_no
let getConsoleScreenBufferInfo = that's_a_no_no
let setConsoleTextAttribute _ = that's_a_no_no
let fillConsoleOutputCharacter _ _ _ = that's_a_no_no
let getConsoleMode = that's_a_no_no
let setConsoleMode _ = that's_a_no_no
let getWindowsVersion = that's_a_no_no
let isWoW64 () = false
let waitpids _ = that's_a_no_no
let writeRegistry _ _ _ = that's_a_no_no
let getConsoleOutputCP = that's_a_no_no
let getCurrentConsoleFontEx _ = that's_a_no_no
let create_glyph_checker = that's_a_no_no
let delete_glyph_checker = that's_a_no_no
let has_glyph _ = that's_a_no_no
let isWoW64Process = that's_a_no_no
let process_putenv _ = that's_a_no_no
let shGetFolderPath _ = that's_a_no_no
let sendMessageTimeout _ _ _ _ _ = that's_a_no_no
let getParentProcessID = that's_a_no_no
let getProcessName = that's_a_no_no
let getConsoleAlias _ = that's_a_no_no
let win_create_process _ _ _ _ _ = that's_a_no_no
