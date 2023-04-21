(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** OS-specific functions requiring C code on at least one platform.

    Most functions are Windows-specific and raise an exception on other
    platforms. *)

include module type of struct include OpamStubsTypes end

val getpid : unit -> int
  (** On Windows, this returns the actual process ID, rather than the non-unique
      faked process ID returned by the Microsoft C Runtime
      (see https://caml.inria.fr/mantis/view.php?id=4034).

      On all other platforms, this is just an alias for [Unix.getpid]. *)

val getCurrentProcessID : unit -> int32
  (** Windows only. As {!getpid}, but without the possibility of truncating the
      ID on 32-bit platforms. *)

val getStdHandle : stdhandle -> handle
  (** Windows only. Return a standard handle. *)

val getConsoleScreenBufferInfo : handle -> console_screen_buffer_info
  (** Windows only. Return current Console screen buffer information. *)

val setConsoleTextAttribute : handle -> int -> unit
  (** Windows only. Set the console's text attribute setting. *)

val fillConsoleOutputCharacter : handle -> char -> int -> int * int -> bool
  (** Windows only. [fillConsoleOutputCharacter buffer c n (x, y)] writes [c]
      [n] times starting at the given coordinate (and wrapping if required). *)

val getConsoleMode : handle -> int
  (** Windows only. Returns the input/output mode of the console screen buffer
      referred to by the handle.

      @raise Not_found If the handle does not refer to a console. *)

val setConsoleMode : handle -> int -> bool
  (** Windows only. Sets the input/output mode of the console screen buffer
      referred to by the handle, returning [true] if the operation isr
      successful. *)

val getWindowsVersion : unit -> int * int * int * int
  (** Windows only. Returns the Windows version as
      [(major, minor, build, revision)]. This function only works if opam is
      compiled OCaml 4.06.0 or later, it returns [(0, 0, 0, 0)] otherwise. *)

val isWoW64 : unit -> bool
  (** Returns [false] unless this process is a 32-bit Windows process running
      in the WoW64 sub-system (i.e. is being run on 64-bit Windows). *)

val waitpids : int list -> int -> int * Unix.process_status
  (** Windows only. Given a list [pids] with [length] elements,
      [waitpids pids length] behaves like [Unix.wait], returning the pid and
      exit status of the first process to terminate. *)

val writeRegistry :
  registry_root -> string -> string -> 'a registry_value -> 'a -> unit
  (** Windows only. [writeRegistry root key name value_type value] sets the
      value [name] of type [value_type] in registry key [key] of [root] to
      [value].

      @raise Failure If the value could not be set.
      @raise Not_found If [key] does not exist. *)

val getConsoleOutputCP : unit -> int
(** Windows only. Retrieves the current Console Output Code Page. *)

val getCurrentConsoleFontEx : handle -> bool -> console_font_infoex
(** Windows only. Gets information on the current console output font. *)

val create_glyph_checker : string -> handle * handle
(** Windows only. Given a font name, returns a pair consisting of a screen DC
    and a font object, which will have been selected into the DC.

    @raise Failure If anything goes wrong with the GDI calls. *)

val delete_glyph_checker : handle * handle -> unit
(** Windows only. Given [(dc, font)], deletes the font object and releases the
    DC. *)

val has_glyph : handle * handle -> Uchar.t -> bool
(** Windows only. [has_glyph (dc, font) scalar] returns [true] if [font]
    contains a glyph for [scalar].

    @raise Failure If the call to [GetGlyphIndicesW] fails. *)

val isWoW64Process : int32 -> bool
(** Windows only. General version of {!isWoW64} for any given process ID. See
    https://msdn.microsoft.com/en-us/library/windows/desktop/ms684139.aspx *)

val process_putenv : int32 -> string -> string -> bool
(** Windows only. [process_putenv pid name value] sets the environment variable
    [name] to [value] in given process ID ([Unix.putenv] must also be called to
    update the value in the current process). This function must not be called
    if the target process is 32-bit and the current process is 64-bit or vice
    versa (outcomes vary from a no-op to a segfault). *)

val shGetFolderPath : int -> shGFP_type -> string
(** Windows only. [shGetFolderPath nFolder dwFlags] retrieves the location of a special
    folder by CSIDL value. See https://msdn.microsoft.com/en-us/library/windows/desktop/bb762181.aspx *)

val sendMessageTimeout :
  nativeint -> int -> int -> ('a, 'b, 'c) winmessage -> 'a -> 'b -> int * 'c
(** Windows only. [sendMessageTimeout hwnd timeout flags message wParam lParam]
    sends a message to the given handle, but is guaranteed to return within
    [timeout] milliseconds. The result consists of two parts, [fst]  is the
    return value from SendMessageTimeout, [snd] depends on both the message and
    [fst]. See https://msdn.microsoft.com/en-us/library/windows/desktop/ms644952.aspx *)

val getParentProcessID : int32 -> int32
(** Windows only. [getParentProcessID pid] returns the process ID of the parent
    of [pid].

    @raise Failure If walking the process tree fails to find the process. *)

val getProcessName : int32 -> string
(** Windows only. [getProcessName pid] returns the executable name of [pid].

    @raise Failure If the process does not exist. *)

val getConsoleAlias : string -> string -> string
(** Windows only. [getConsoleAlias alias exeName] retrieves the value for a
    given executable or [""] if the alias is not defined. See
    https://docs.microsoft.com/en-us/windows/console/getconsolealias *)

val win_create_process : string -> string -> string option -> Unix.file_descr ->
                         Unix.file_descr -> Unix.file_descr -> int
(** Windows only. Provided by OCaml's win32unix library. *)
