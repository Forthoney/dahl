structure Reader: READER =
struct
  fun map f rdr strm =
    case rdr strm of
      SOME (v, strm) => SOME (f v, strm)
    | NONE => NONE

  fun app f rdr strm =
    case rdr strm of
      SOME (v, strm) => (f v; app f rdr strm)
    | NONE => ()

  fun inspect f rdr strm =
    case rdr strm of
      SOME (v, strm) => SOME ((ignore (f v); v), strm)
    | NONE => NONE

  fun repeat scanner {between = NONE} strm =
      let
        fun loop acc strm =
          case scanner strm of
            NONE => (rev acc, strm)
          | SOME (v, strm) => loop (v :: acc) strm
      in
        loop [] strm
      end
    | repeat scanner {between = SOME btw} strm =
      let
        fun loop acc strm =
          case btw strm of
            NONE => (rev acc, strm)
          | SOME strm' =>
              case scanner strm' of
                NONE => (rev acc, strm')
              | SOME (v, strm) => loop (v :: acc) strm
      in
        case scanner strm of
          NONE => ([], strm)
        | SOME (v, strm) => loop [v] strm
      end
end
