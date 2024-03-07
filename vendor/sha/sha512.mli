(*
 *	Copyright (C) 2006-2009 Vincent Hanquez <tab@snarc.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** SHA512 OCaml binding *)

(** Context type - opaque. *)
type ctx

(** Buffer type. *)
type buf = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Digest type - opaque. *)
type t

(** The zero digest. *)
val zero : t

(** Create a new context. *)
external init : unit -> ctx = "stub_sha512_init"

(** [Sha512.unsafe_update_substring ctx s ofs len] updates the context with the
   substring of [s] starting at character number [ofs] and containing [len]
   characters. Unsafe: No range checking! *)
external unsafe_update_substring : ctx -> string -> int -> int -> unit = "stub_sha512_update"

(** [Sha512.update_substring ctx s ofs len] updates the context with the
   substring of [s] starting at character number [ofs] and containing [len]
   characters. *)
val update_substring : ctx -> string -> int -> int -> unit

(** [Sha512.update_string ctx s] updates the context with [s]. *)
val update_string : ctx -> string -> unit

(** [Sha512.update_buffer ctx a] updates the context with [a]. Runs parallel to
   other threads if any exist. *)
external update_buffer : ctx -> buf -> unit = "stub_sha512_update_bigarray"

(** Finalize the context and return digest. *)
external finalize : ctx -> t = "stub_sha512_finalize"

(** Return a copy of the context. *)
external copy : ctx -> ctx = "stub_sha512_copy"

(** Return the digest of the given string. *)
val string : string -> t

(** [Sha512.substring s ofs len] returns the digest of the substring of [s]
   starting at character number [ofs] and containing [len] characters. *)
val substring : string -> int -> int -> t

(** Return the digest of the given buffer. *)
val buffer : buf -> t

(** If [len] is nonnegative, [Sha512.channel ic len] reads [len] characters from
   channel [ic] and returns their digest, or raises [End_of_file] if end-of-file
   is reached before [len] characters are read. If [len] is negative,
   [Sha512.channel ic len] reads all characters from [ic] until end-of-file is
   reached and return their digest. *)
val channel : in_channel -> int -> t

(** Return the digest of the file whose name is given. *)
val file : string -> t

(** Return the digest of the file whose name is given using fast C function. *)
val file_fast : string -> t

(** Write a digest on the given output channel. *)
val output : out_channel -> t -> unit

(** Read a digest from the given input channel. *)
val input : in_channel -> t

(** Return a binary representation of the given digest. *)
val to_bin : t -> string

(** Return a printable hexadecimal representation of the given digest. *)
val to_hex : t -> string

(** Returns whether two hashes are equal. *)
val equal : t -> t -> bool

(** Sha512.of_bin digest converts the binary representation of a digest to the
   internal representation of Sha512.t. *)
val of_bin : bytes -> t

(** Sha512.of_hex digest converts the hexadecimal representation of a digest to
   the internal representation of Sha512.t. *)
val of_hex : string -> t
