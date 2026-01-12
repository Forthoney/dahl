functor HashTable (
  structure Key : HASHABLE

  val maxLoad : real
  val slopFactor : real
) : HASH_TABLE =
struct
  type k = Key.t

  datatype 'v entry =
    Empty
  | Tombstone
  | Live of k * 'v

  datatype 'v probe_result =
    Occupied of int * 'v
  (* If the available slot is an empty slot, returns idx
     If the available slot is a tombstone slot, returns ~idx - 1 *)
  | Available of int 
  | Nothing (* there is nothing in the table *)

  type 'v t = {count : int ref, entries : 'v entry array ref}

  val hash = Word64.toIntX o Key.hash

  fun new capacity = {count = ref 0, entries = ref (Array.array (capacity, Empty))}
  fun empty () = new 0

  fun atCapacity {count, entries} =
    Real.fromInt (! count + 1) > Real.fromInt (Array.length (! entries)) * maxLoad

  fun toTomb idx = ~idx - 1
  fun fromTomb idx = ~(idx + 1)

  fun probe entries k =
    case Array.length entries of
      0 => Nothing
    | len =>
      let
        fun loop tomb idx =
          case Array.sub (entries, idx) of
            Tombstone =>
            loop (if tomb = 0 then toTomb idx else tomb) (idx + 1 mod len)
          | Empty => Available (if tomb = 0 then idx else tomb)
          | Live (k', v) =>
            if Key.eq (k', k) then Occupied (idx, v)
            else loop tomb (idx + 1 mod len)
      in
        loop 0 (hash k mod len)
      end

  fun copy (src, dest) =
    let
      fun rehash (Empty | Tombstone) = ()
        | rehash (Live (k, v)) =
          case probe dest k of
            Occupied (idx, _) => Array.update (dest, idx, Live (k, v))
          (* no deletions have happened yet, so no tombstones *)
          | Available idx => Array.update (dest, idx, Live (k, v))
          | Nothing => raise Size
    in
      Array.app rehash src
    end
    
  fun resize {count, entries} capacity =
    let
      val _ = if capacity < ! count then raise Size else ()
      val entries' = Array.array (capacity, Empty)
    in
      (copy (! entries, entries'); entries := entries')
    end

  fun insert (tbl as {count, entries}) (k, v) =
    ( if atCapacity tbl then
        resize tbl (Real.ceil (Real.fromInt (Array.length (! entries)) * slopFactor))
      else ()
    ; case probe (! entries) k of
        Nothing => raise Fail "unreachable"
      | Occupied (idx, _) => Array.update (! entries, idx, Live (k, v))
      | Available idx =>
        if idx >= 0 then
          (Array.update (! entries, idx, Live (k, v)); count := ! count + 1)
        else
          Array.update (! entries, fromTomb idx, Live (k, v))
    )

  fun get {count, entries} k =
    case probe (! entries) k of
      Nothing | Available _ => NONE
    | Occupied (_, v) => SOME v

  fun take {count, entries} k =
    case probe (! entries) k of
      Nothing | Available _ => NONE
    | Occupied (idx, v) => (Array.update (! entries, idx, Tombstone); SOME v)

  fun count {count, entries} =
    Array.foldl (fn (Live _, acc) => acc + 1 | (_, acc) => acc) 0 (! entries)
end
