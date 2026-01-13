signature INTERNER =
sig
  type v
  type t

  val new : unit -> t
  val get : t -> v -> int
  val freeze : t -> v vector
end
