functor IDMapFn (HT : HASH_TABLE) :> ID_MAP
where type v = HT.k =
struct
  type id = int
  type v = HT.k
  type t = id HT.t

  fun new () = HT.new 100

  fun get tbl v =
    case HT.get tbl v of
      SOME id => id
    | NONE =>
      let val newId = HT.count tbl
      in (HT.insert tbl (v, newId); newId)
      end

  fun freeze tbl =
    let
      val len = HT.count tbl
      val a = Array.array (len, NONE)
    in
      ( HT.app (fn (v, idx) => Array.update (a, idx, SOME v)) tbl
      ; Vector.tabulate (len, (fn idx => Option.valOf (Array.sub (a, idx))))
      )
    end
end
