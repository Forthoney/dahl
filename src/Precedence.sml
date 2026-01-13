structure Precedence =
struct
  type t = int
  val assign = 1
  val or = 2
  val and' = 3
  val eq = 4
  val cmp = 5
  val concat = 6
  val term = 7
  val factor = 8
  val unary = 9
  val pow = 10
  val call = 11
  val primary = 12

  structure L = Lexer
  fun ofToken token =
    case token of
      L.SUB | L.ADD => term
    | L.MUL | L.DIV | L.MOD => factor
    | L.NE | L.EQ => eq
    | L.LE | L.LT | L.GE | L.GT => cmp
    | _ => 0
end

