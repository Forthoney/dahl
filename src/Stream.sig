signature CHAR_STREAM_IO =
sig
  type state
  val mk : TextIO.StreamIO.instream -> state
  val flatten :
    (string, TextIO.StreamIO.instream) StringCvt.reader
    -> (char, state) StringCvt.reader
end

signature STREAM =
sig
  type ('a, 's) reader = ('a, 's) StringCvt.reader
  val map : ('a -> 'b) -> ('a, 's) reader -> ('b, 's) reader
  val app : ('a -> 'b) -> ('a, 's) reader -> (unit, 's) reader
  val fold : ('a * 'b -> 'b) -> 'b -> ('a, 's) reader -> 's -> 'b
  val collect : ('a, 's) reader -> 's -> 'a list
  val force : ('a, 's) reader -> 's -> unit
  val nest : ('a, 's1) reader * ('s1, 's2) reader -> ('a, 's1 * 's2) reader

  structure Char : CHAR_STREAM_IO
end
