functor HashTableFn (
  structure Key : HASHABLE
  val maxLoad : real
  val slopFactor : real
) :> HASH_TABLE
where type k = Key.t =
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

  type 'v t =
    { live : int ref
    , tomb : int ref
    , entries : 'v entry array ref
    }

  val hash = Word64.toIntX o Key.hash

  fun new capacity =
    { live = ref 0
    , tomb = ref 0
    , entries = ref (Array.array (capacity, Empty))
    }
  fun empty () = new 0

  fun atCapacity {live, tomb, entries} =
    Real.fromInt (! live + ! tomb + 1) > Real.fromInt (Array.length (! entries)) * maxLoad

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
          (* no deletions have happened yet, so no tombstones *)
            Available idx => Array.update (dest, idx, Live (k, v))
          | Nothing => raise Fail "unreachable: dest is of nonzero size"
          | Occupied (idx, _) => raise Fail "unreachable: each key is rehashed once"
    in
      Array.app rehash src
    end
    
  fun count {live, tomb, entries} = ! live

  fun resize {live, tomb, entries} capacity =
    if capacity >= ! live then
      let val entries' = Array.array (capacity, Empty)
      in
        ( copy (! entries, entries')
        ; entries := entries'
        ; tomb := 0
        )
      end
    else raise Size

  fun autoSize entries =
    Real.ceil (Real.fromInt (Array.length (! entries)) * slopFactor)

  fun insert (tbl as {live, tomb, entries}) (k, v) =
    let
      val _ = if atCapacity tbl then resize tbl (autoSize entries) else ()
      val idx =
        case probe (! entries) k of
          Nothing => raise Fail "unreachable: resize ensures nonzero capacity"
        | Occupied (idx, _) => idx
        | Available idx =>
          if idx >= 0 then (live := ! live + 1; idx)
          else (live := ! live + 1; tomb := ! tomb - 1; fromTomb idx)
    in
      Array.update (! entries, idx, Live (k, v))
    end

  fun get {live, tomb, entries} k =
    case probe (! entries) k of
      Nothing | Available _ => NONE
    | Occupied (_, v) => SOME v

  fun take {live, tomb, entries} k =
    case probe (! entries) k of
      Nothing | Available _ => NONE
    | Occupied (idx, v) =>
      ( Array.update (! entries, idx, Tombstone)
      ; live := ! live - 1
      ; tomb := ! tomb + 1
      ; SOME v
      )

  fun app f {live, tomb, entries} =
    Array.app (fn (Live ent) => f ent | _ => ()) (! entries)
end
