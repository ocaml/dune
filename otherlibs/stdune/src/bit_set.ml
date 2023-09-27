module Common = struct
  (* All of these values are the same regardless of the instantiation of the
     functor. That is to say, they're independent of the representation of
     elements *)

  let empty = 0
  let union = ( lor )
  let inter = ( land )
  let equal = Int.equal
  let compare = Int.compare
end

module Make (Element : sig
    type t

    val to_int : t -> int
    val all : t list
    val to_dyn : t -> Dyn.t
  end) =
struct
  type t = int

  include Common

  let () =
    assert (List.length Element.all < Sys.int_size);
    let (_ : unit Int.Map.t) =
      Int.Map.of_list_map_exn Element.all ~f:(fun x ->
        let x = Element.to_int x in
        assert (x >= 0 && x < Sys.int_size);
        x, ())
    in
    ()
  ;;

  let singleton x = 1 lsl Element.to_int x
  let add t x = union t (singleton x)
  let mem t x = t land singleton x <> empty

  let to_dyn t : Dyn.t =
    Set
      (List.fold_left Element.all ~init:[] ~f:(fun acc x ->
         if mem t x then Element.to_dyn x :: acc else acc))
  ;;

  let of_func =
    let all = Array.of_list Element.all in
    fun f ->
      let acc = ref empty in
      for i = 0 to Array.length all - 1 do
        if f all.(i) then acc := union !acc (singleton all.(i))
      done;
      !acc
  ;;
end
