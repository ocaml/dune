(* $Id: fl_metacache_unix.ml,v 1.2 2002/09/22 20:12:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

let remove_dups_from_path p =
  (* Removes directories which are physically the same from the path [p],
   * and returns the shortened path
   *)

  let dir_identity = Hashtbl.create 20 in

  let rec remove p =
    match p with
	d :: p' ->
	  begin try
	    let s = Unix.stat d in
	    let id = (s.Unix.st_dev, s.Unix.st_ino) in
	    if Hashtbl.mem dir_identity id then
	      remove p'
	    else begin
	      Hashtbl.add dir_identity id ();
	      d :: (remove p')
	    end
	  with error ->
	    (* Don't know anything, so the "directory" remains in the path *)
	    d :: (remove p')
	  end
      | [] ->
	  []
  in

  remove p
;;
