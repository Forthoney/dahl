signature HASH_TABLE =
sig
  type k
  type 'v t

  val empty : unit -> 'v t
  val new : int -> 'v t

  val insert : 'v t -> (k * 'v) -> unit
  val get : 'v t -> k -> 'v option
  val take : 'v t -> k -> 'v option

  (* the count of values in the table *)
  val count : 'v t -> int
  val resize : 'v t -> int -> unit
end

