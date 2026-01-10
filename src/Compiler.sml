structure Compiler =
struct
  type 's state =
    { lexState : 's Lexer.state 
    , hadError : bool
    }

  fun run strm =
    let
      fun charRdr strm = Stream.Char.flatten TextIO.StreamIO.inputN (strm, 4096)
      val tokenRdr = Lexer.run charRdr
      val lexState = (Lexer.mk o Stream.Char.mk o TextIO.getInstream) strm
      fun advance {lexState, hadError} =
        case tokenRdr lexState of
          SOME (token, s) => SOME (token, {lexState = s, hadError})
        | NONE => NONE
    in    
      ()
    end
end
