signature ID_MAP =
sig
  type t
  type v
  type id

  structure Builder :
  sig
    type obj
    val new : unit -> obj
    val get : obj -> v -> id
    val freeze : obj -> t
  end

  val get : t -> id -> v
  val idToString : id -> string
end
