signature READER =
sig
  val map: ('a -> 'b)
           -> ('a, 'strm) StringCvt.reader
           -> ('b, 'strm) StringCvt.reader

  (* `app f scanner` eagerly applies `f` to the items produced by scanner *)
  val app: ('a -> unit) -> ('a, 'strm) StringCvt.reader -> 'strm -> unit

  (* `inspect f scanner` applies f to the item produced by scanner,
     but does not modify the item otherwise. It is expected that `f` is side-effectful *)
  val inspect: ('a -> 'b)
               -> ('a, 'strm) StringCvt.reader
               -> ('a, 'strm) StringCvt.reader

  (* Repeat the scanner until failure with an optional separator between each element *)
  val repeat:
    ('a, 'strm) StringCvt.reader
    -> {between: ('strm -> 'strm option) option}
    (* Unlike a true StringCvt.reader,
       when nothing matches the scanner, this pseudo-reader returns an emtpy list
       instead of returning a NONE option *)
    -> ('strm -> 'a list * 'strm)
end
