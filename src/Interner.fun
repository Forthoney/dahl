functor Interner (
  structure HT : HASH_TABLE
) =
struct
  val new () = HT.new 100
  fun get tbl v =
    case HT.get tbl v of
      SOME id => id
    | NONE =>
      let val newId = HT.count tbl
      in (insert tbl newId; newId)
      end

  fun freeze tbl =
    let
      val len = HT.count tbl
      val a = Array.array (len, NONE)
    in
      ( HT.app (fn (v, idx) => Array.update (a, idx, SOME v)) tbl
      ; Vector.tabulate (len, (fn i => Option.valOf (Array.sub (a, idx))))
      )
    end
end
