(* $Id: fl_split.ml,v 1.1 2002/09/22 13:32:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


let in_words s =
  (* splits s in words separated by commas and/or whitespace *)
  let l = String.length s in
  let rec split i j =
    if j < l then
      match s.[j] with
	(' '|'\t'|'\n'|'\r'|',') ->
	  if i<j then (String.sub s i (j-i)) :: (split (j+1) (j+1))
		 else split (j+1) (j+1)
      |	_ ->
	  split i (j+1)
    else
      if i<j then [ String.sub s i (j-i) ] else []
  in
  split 0 0
;;


let in_words_ws s =
  (* splits s in words separated by whitespace *)
  let l = String.length s in
  let rec split i j =
    if j < l then
      match s.[j] with
	(' '|'\t'|'\n'|'\r') ->
	  if i<j then (String.sub s i (j-i)) :: (split (j+1) (j+1))
		 else split (j+1) (j+1)
      |	_ ->
	  split i (j+1)
    else
      if i<j then [ String.sub s i (j-i) ] else []
  in
  split 0 0
;;


let path_separator =
  match Sys.os_type with
      "Unix"   -> ':'
    | "Cygwin" -> ';'   (* You might want to change this *)
    | "Win32"  -> ';'
    | "MacOS"  -> failwith "Findlib: I do not know what is the correct path separator for MacOS. If you can help me, write a mail to gerd@gerd-stolpmann.de"
    | _ -> failwith "Findlib: unknown operating system"
;;


let path str =
  (* split "str" into parts separated by "path_separator" *)
  let l = String.length str in
  let rec split_up j k =
    if k < l then begin
      let c = str.[k] in
      if c = path_separator then begin
        if k - j > 0 then
	  String.sub str j (k-j) :: split_up (k+1) (k+1)
        else
	  split_up (k+1) (k+1)
      end
      else
	split_up j (k+1)
    end
    else
      if k - j > 0 then
        [ String.sub str j (k-j) ]
      else
	[]
  in
  split_up 0 0
;;


let norm_dir d =
  (* Converts the file name of the directory [d] to the normal form.
   * For Unix, the '/' characters at the end are removed, and multiple
   * '/' are deleted.
   * For Windows, all '/' characters are converted to '\'. Two
   * backslashes at the beginning are tolerated.
   *)
  let s = String.copy d in
  let l = String.length d in
  let norm_dir_unix() =
    for k = 1 to l - 1 do
      if s.[k] = '/' && s.[k-1] = '/' then s.[k] <- Char.chr 0;
      if s.[k] = '/' && k = l-1 then s.[k] <- Char.chr 0
    done
  in
  let norm_dir_win() =
    if l >= 1 && s.[0] = '/' then s.[0] <- '\\';
    if l >= 2 && s.[1] = '/' then s.[1] <- '\\';
    for k = 2 to l - 1 do
      if s.[k] = '/' then s.[k] <- '\\';
      if s.[k] = '\\' && s.[k-1] = '\\' then s.[k] <- Char.chr 0;
      if s.[k] = '\\' && k = l-1 then s.[k] <- Char.chr 0
    done
  in
  let expunge() =
    let n = ref 0 in
    for k = 0 to l - 1 do
      if s.[k] = Char.chr 0 then incr n
    done;
    let s' = String.create (l - !n) in
    n := 0;
    for k = 0 to l - 1 do
      if s.[k] <> Char.chr 0 then begin
	s'.[ !n ] <- s.[k];
	incr n
      end
    done;
    s'
  in
  match Sys.os_type with
      "Unix" | "Cygwin" -> norm_dir_unix(); expunge()
    | "Win32" -> norm_dir_win(); expunge()
    | _ -> failwith "This os_type is not supported"
;;

(* ======================================================================
 * History:
 * 
 * $Log: fl_split.ml,v $
 * Revision 1.1  2002/09/22 13:32:32  gerd
 * 	Renamed file from split.ml to fl_split.ml to avoid
 * name clashes
 *
 * ======================================================================
 * OLD LOGS FOR split.ml:
 *
 * Revision 1.3  2001/10/12 20:16:08  gerd
 * 	New: norm_dir.
 *
 * Revision 1.2  2001/02/24 20:22:47  gerd
 * 	Split.path: Support for the cygwin port.
 *
 * Revision 1.1  1999/06/20 19:26:26  gerd
 * 	Major change: Added support for META files. In META files, knowlege
 * about compilation options, and dependencies on other packages can be stored.
 * The "ocamlfind query" subcommand has been extended in order to have a
 * direct interface for that. "ocamlfind ocamlc/ocamlopt/ocamlmktop/ocamlcp"
 * subcommands have been added to simplify the invocation of the compiler.
 *
 * 
 *)
