structure Stream :> STREAM =
struct
  type ('a, 's) reader = ('a, 's) StringCvt.reader

  fun map f rdr s =
    case rdr s of
      NONE => NONE
    | SOME (x, s) => SOME (f x, s)

  fun app f = map (ignore o f)

  fun fold f z rdr s =
    let
      fun loop acc s =
        case rdr s of
          NONE => acc
        | SOME (x, s) => loop (f (x, acc)) s
    in
      loop z s
    end

  fun collect rdr =
    let
      fun loop acc s =
        case rdr s of
          NONE => rev acc
        | SOME (x, s) => loop (x :: acc) s
    in
      loop []
    end

  fun force rdr =
    let
      fun loop s = 
        case rdr s of
          NONE => ()
        | SOME (_, s) => loop s
    in
      loop
    end

  fun nest (inner, outer) (sInner, sOuter) =
    case inner sInner of
      SOME (v, sInner) => SOME (v, (sInner, sOuter))
    | NONE =>
      case outer sOuter of
        SOME s => nest (inner, outer) s
      | NONE => NONE

  structure Char :> CHAR_STREAM_IO =
  struct
    type state = substring * TextIO.StreamIO.instream
    fun mk instrm = (Substring.full "", instrm)
    fun flatten strmRdr =
      nest (Substring.getc, map Substring.full strmRdr)
  end
end
