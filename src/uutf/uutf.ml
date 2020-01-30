module String = struct
  let unsafe_byte str i = Char.code (String.unsafe_get str i)

  let rec fold_rec f acc str ofs =
    if ofs = String.length str then
      acc
    else
      let ch = String.unsafe_get str ofs in
      match ch with
      | '\x00' .. '\x7f' -> fold_uchar f acc str ofs 1 (Uchar.of_char ch)
      | '\xc0' .. '\xdf' ->
        if ofs + 2 > String.length str then
          fold_unfinished f acc str ofs
        else
          fold_uchar f acc str ofs 2
            (Uchar.of_int
               ( ((Char.code ch land 0x1f) lsl 6)
               lor (unsafe_byte str (ofs + 1) land 0x3f) ))
      | '\xe0' .. '\xef' ->
        if ofs + 3 > String.length str then
          fold_unfinished f acc str ofs
        else
          fold_uchar f acc str ofs 3
            (Uchar.of_int
               ( ((Char.code ch land 0x0f) lsl 12)
               lor ((unsafe_byte str (ofs + 1) land 0x3f) lsl 6)
               lor (unsafe_byte str (ofs + 2) land 0x3f) ))
      | '\xf0' .. '\xf7' ->
        if ofs + 4 > String.length str then
          fold_unfinished f acc str ofs
        else
          fold_uchar f acc str ofs 4
            (Uchar.of_int
               ( ((Char.code ch land 0x07) lsl 18)
               lor ((unsafe_byte str (ofs + 1) land 0x3f) lsl 12)
               lor ((unsafe_byte str (ofs + 2) land 0x3f) lsl 6)
               lor (unsafe_byte str (ofs + 3) land 0x3f) ))
      | _ ->
        let s = String.sub str ofs 1 in
        let acc = f acc ofs (`Malformed s) in
        fold_rec f acc str (ofs + 1)

  and fold_uchar f acc str ofs size ch =
    let acc = f acc ofs (`Uchar ch) in
    fold_rec f acc str (ofs + size)

  and[@inlined never] fold_unfinished f acc str ofs =
    let s = String.sub str ofs (String.length str - ofs) in
    f acc ofs (`Malformed s)

  let fold_utf_8 f acc str = fold_rec f acc str 0
end

module Buffer = struct
  let add_utf_8 = Buffer.add_utf_8_uchar
end
