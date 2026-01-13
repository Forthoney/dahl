signature HASH_TABLE =
sig
  type k
  type 'v t

  val empty : unit -> 'v t
  (* initialize a hash table with the specified capacity *)
  val new : int -> 'v t

  val insert : 'v t -> (k * 'v) -> unit
  (* retrieve the value associated with a key *)
  val get : 'v t -> k -> 'v option
  (* delete an entry associated with a key and return the value *)
  val take : 'v t -> k -> 'v option

  (* the count of valid values in the table *)
  val count : 'v t -> int
  (* resize the capacity of the hashset to the specified size
     raises Size if the requested size is too small *)
  val resize : 'v t -> int -> unit

  val app : (k * 'v -> unit) -> 'v t -> unit
end
