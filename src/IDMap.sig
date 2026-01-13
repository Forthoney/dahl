signature ID_MAP =
sig
  type id = int
  type v
  type t

  val new : unit -> t
  val get : t -> v -> id
  val freeze : t -> v vector
end
