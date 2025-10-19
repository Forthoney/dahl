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

  fun repeat scanner sep strm =
    case sep of
      {sep = NONE} =>
        let
          fun loop acc strm =
            case scanner strm of
              NONE => (rev acc, strm)
            | SOME (v, strm) => loop (v :: acc) strm
        in
          loop [] strm
        end
    | {sep = SOME sep} =>
        let
          fun loop acc strm =
            case sep strm of
              NONE => (rev acc, strm)
            | SOME strm' =>
                case scanner strm' of
                  NONE => (rev acc, strm)
                | SOME (v, strm) => loop (v :: acc) strm
        in
          case scanner strm of
            NONE => ([], strm)
          | SOME (v, strm) => loop [v] strm
        end
end
